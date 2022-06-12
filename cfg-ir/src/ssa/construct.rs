use std::{borrow::BorrowMut, collections::HashMap};

use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::dominators::{
        compute_dominance_frontiers, compute_immediate_dominators, dominator_tree,
    },
    Graph, NodeId,
};

use crate::{
    builder::Builder,
    function::Function,
    instruction::{
        location::{InstructionIndex, InstructionLocation},
        Phi,
    },
    value::{self, ValueId},
};

use super::error::Error;

pub fn construct(function: &mut Function) -> Result<(), Error> {
    let entry = function
        .graph()
        .entry()
        .ok_or(Error::Graph(graph::Error::NoEntry))?;
    let mut dominance_frontiers = compute_dominance_frontiers(function.graph(), entry, None)?;
    dominance_frontiers.retain(|_, f| !f.is_empty());
    println!("{:#?}", dominance_frontiers);
    let mut node_to_values_written = FxHashMap::default();
    let mut value_written_to_nodes = FxHashMap::default();
    for &node in dominance_frontiers.keys() {
        let mut builder = Builder::new(function);
        let mut block = builder.block(node).unwrap();
        for instruction_index in block.instruction_indexes() {
            let instruction = block.instruction(instruction_index).unwrap();
            let values_written = instruction.values_written();
            for &value in values_written.iter() {
                value_written_to_nodes
                    .entry(value)
                    .or_insert_with(Vec::new)
                    .push(node);
            }
            node_to_values_written
                .entry(node)
                .or_insert_with(FxHashSet::<ValueId>::default)
                .borrow_mut()
                .extend(values_written.iter())
        }
    }

    let mut value_to_nodes_with_phi = FxHashMap::<ValueId, FxHashSet<NodeId>>::default();
    for (&value, nodes) in &mut value_written_to_nodes {
        while let Some(node) = nodes.pop() {
            let nodes_with_phi = value_to_nodes_with_phi
                .entry(value)
                .or_insert_with(FxHashSet::default)
                .borrow_mut();
            if let Some(frontiers) = dominance_frontiers.get(&node) {
                for &dominance_frontier_node in frontiers {
                    if !nodes_with_phi.contains(&dominance_frontier_node) {
                        let incoming_values = function
                            .graph()
                            .predecessors(dominance_frontier_node)
                            .map(|p| (p, value))
                            .collect::<FxHashMap<_, _>>();
                        Builder::new(function)
                            .block(dominance_frontier_node)
                            .unwrap()
                            .push_phi(Phi {
                                dest: value,
                                incoming_values,
                            });
                        nodes_with_phi.insert(dominance_frontier_node);
                        match node_to_values_written.get(&dominance_frontier_node) {
                            Some(values_written) if values_written.contains(&value) => {}
                            _ => nodes.push(dominance_frontier_node),
                        }
                    }
                }
            }
        }
    }

    fn split_values(
        function: &mut Function,
        node: NodeId,
        dominator_tree: &Graph,
        value_stacks: &mut FxHashMap<ValueId, Vec<ValueId>>,
    ) {
        let old_value_stacks = value_stacks.clone();

        let mut builder = Builder::new(function);
        let block = builder.block(node).unwrap();
        for instruction_index in block.instruction_indexes() {
            let mut builder = Builder::new(function);
            let mut block = builder.block(node).unwrap();
            let mut instruction = block.instruction(instruction_index).unwrap();

            if !matches!(instruction_index, InstructionIndex::Phi(_)) {
                for (read_value_index, &value) in instruction.values_read().iter().enumerate() {
                    if let Some(value_stack) = value_stacks.get(&value) {
                        instruction
                            .replace_value_read(read_value_index, *value_stack.last().unwrap())
                            .unwrap();
                    }
                }
            }

            for (written_value_index, &value) in instruction.values_written().iter().enumerate() {
                if let Some(value_stack) = value_stacks.get_mut(&value) {
                    let new_value = function.new_value();
                    value_stack.push(new_value);

                    let mut builder = Builder::new(function);
                    let mut block = builder.block(node).unwrap();
                    let mut instruction = block.instruction(instruction_index).unwrap();
                    instruction
                        .replace_value_written(written_value_index, new_value)
                        .unwrap();
                } else {
                    value_stacks.insert(value, vec![value]);
                }
            }
        }

        for successor in function.graph().successors(node).collect::<Vec<_>>() {
            let mut builder = Builder::new(function);
            let mut block = builder.block(successor).unwrap();
            let mut phis = block.phi_instructions().clone();
            block.clear_phi_instructions();

            for mut phi in phis.drain(..) {
                let old_value = phi.incoming_values[&node];
                if let Some(value_stack) = value_stacks.get(&old_value) {
                    phi.incoming_values
                        .insert(node, *value_stack.last().unwrap());
                }

                block.push_phi(phi);
            }
        }

        // TODO: can we use compute_bfs_order instead?
        // TODO: rename immediate_dominators to immediately_dominates?
        for child_block in dominator_tree.successors(node) {
            split_values(function, child_block, dominator_tree, value_stacks);
        }

        *value_stacks = old_value_stacks;
    }

    split_values(
        function,
        entry,
        &mut dominator_tree(function.graph(), entry)?,
        &mut FxHashMap::default(),
    );

    // TODO: split_values fucks DefUse

    // TODO: test pruning
    loop {
        let mut phis_to_remove = Vec::<(NodeId, usize)>::new();
        let mut values_to_replace = HashMap::<ValueId, ValueId>::new();

        for node in function.graph().nodes().clone() {
            let mut builder = Builder::new(function);
            let block = builder.block(node).unwrap();
            for (phi_index, phi) in block
                .phi_instructions()
                .iter()
                .cloned()
                .enumerate()
                .rev()
                .collect::<Vec<_>>()
            {
                let unique = phi.incoming_values.values().cloned().collect::<FxHashSet<_>>();
                if unique.len() == 1 {
                    let new_value = *unique.iter().next().unwrap();
                    if new_value != phi.dest {
                        values_to_replace.insert(phi.dest, new_value);
                    }
                    phis_to_remove.push((node, phi_index));
                } else if let Some(def_use_info) = function.def_use.get(phi.dest) {
                    if def_use_info
                        .reads
                        .iter()
                        .filter(|InstructionLocation(other_node, other_instruction_index)| {
                            if *other_node == node {
                                if let InstructionIndex::Phi(other_phi_index) = *other_instruction_index
                                {
                                    return other_phi_index != phi_index;
                                }
                            }

                            true
                        })
                        .count()
                        == 0
                    {
                        phis_to_remove.push((node, phi_index));
                    }
                }
            }
        }

        if phis_to_remove.is_empty() {
            break;
        }

        let mut builder = Builder::new(function);
        for (node, phi_index) in phis_to_remove {
            let mut block = builder.block(node).unwrap();
            // TODO: dont re-calculate DefUse until done pruning
            block.remove(InstructionIndex::Phi(phi_index)).unwrap();
        }

        // we dont need to worry about where to replace since ssa form means values will only be written to once :)
        for node in function.graph().nodes().clone() {
            for (&old, &new) in &values_to_replace {
                let mut builder = Builder::new(function);
                let mut block = builder.block(node).unwrap();
                for instruction_index in block.instruction_indexes() {
                    let mut instruction = block.instruction(instruction_index).unwrap();
                    instruction.replace_values_read(old, new);
                }
            }
        }
    }

    Ok(())
}
