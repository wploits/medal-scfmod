use std::{borrow::{BorrowMut, Cow}, collections::HashMap, time};

use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::dominators::{compute_dominance_frontiers, dominator_tree, compute_immediate_dominators},
    Graph, NodeId,
};

use crate::{
    def_use::DefUse,
    function::Function,
    instruction::{
        location::{InstructionIndex, InstructionLocation},
        value_info::ValueInfo,
        Phi,
    },
    value::ValueId,
};

use super::error::Error;

pub fn construct(function: &mut Function) -> Result<(), Error> {
    let entry = function
        .entry()
        .ok_or(Error::Graph(graph::Error::NoEntry))?;

    let now = time::Instant::now();
    let immediate_dominators = Cow::Owned(compute_immediate_dominators(function.graph(), entry)?);
    let imm_dom_computed = now.elapsed();
    println!("-compute immediate dominators: {:?}", imm_dom_computed);

    let now = time::Instant::now();
    let mut dominance_frontiers = compute_dominance_frontiers(function.graph(), entry, Some(immediate_dominators))?;
    dominance_frontiers.retain(|_, f| !f.is_empty());
    let df_computed = now.elapsed();
    println!("-compute dominance frontiers: {:?}", df_computed);

    let now = time::Instant::now();
    let mut node_to_values_written = FxHashMap::default();
    let mut value_written_to_nodes = FxHashMap::default();
    for &node in dominance_frontiers.keys() {
        let block = function.block(node).unwrap();
        let values_written = block
            .phi_instructions
            .iter()
            .map(|phi| phi.values_written())
            .chain(
                block
                    .inner_instructions
                    .iter()
                    .map(|inner| inner.values_written()),
            )
            .flatten()
            .collect::<Vec<_>>();
        for &value in &values_written {
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
                        function
                            .block_mut(dominance_frontier_node)
                            .unwrap()
                            .phi_instructions
                            .push(Phi {
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
    let phis_inserted = now.elapsed();
    println!("-phi insertation: {:?}", phis_inserted);

    fn split_values(
        function: &mut Function,
        node: NodeId,
        dominator_tree: &Graph,
        value_stacks: &mut FxHashMap<ValueId, Vec<ValueId>>,
    ) {
        let old_value_stacks = value_stacks.clone();

        for index in function.block_mut(node).unwrap().indices() {
            if !matches!(index, InstructionIndex::Phi(_)) {
                let value_info = function
                    .block_mut(node)
                    .unwrap()
                    .value_info_mut(index)
                    .unwrap();
                for (read_value_index, &value) in value_info.values_read().iter().enumerate() {
                    if let Some(value_stack) = value_stacks.get(&value) {
                        *value_info.values_read_mut()[read_value_index] =
                            *value_stack.last().unwrap();
                    }
                }
            }

            let mut values_to_replace = FxHashMap::default();
            for (written_value_index, &value) in function
                .block_mut(node)
                .unwrap()
                .value_info_mut(index)
                .unwrap()
                .values_written()
                .iter()
                .enumerate()
            {
                if let Some(value_stack) = value_stacks.get_mut(&value) {
                    let new_value = function.new_value();
                    value_stack.push(new_value);

                    values_to_replace
                        .entry(node)
                        .or_insert_with(Vec::new)
                        .push((written_value_index, new_value));
                } else {
                    value_stacks.insert(value, vec![value]);
                }
            }

            for (node, values) in values_to_replace {
                let block = function.block_mut(node).unwrap();
                for (written_value_index, value) in values {
                    *block.value_info_mut(index).unwrap().values_written_mut()
                        [written_value_index] = value;
                }
            }

            //let values_written_replace = values_written.iter().enumerate()
        }

        for successor in function.graph().successors(node).collect::<Vec<_>>() {
            let block = function.block_mut(successor).unwrap();

            for phi in &mut block.phi_instructions {
                let old_value = phi.incoming_values[&node];
                if let Some(value_stack) = value_stacks.get(&old_value) {
                    phi.incoming_values
                        .insert(node, *value_stack.last().unwrap());
                }
            }
        }

        for child_block in dominator_tree.successors(node) {
            split_values(function, child_block, dominator_tree, value_stacks);
        }

        *value_stacks = old_value_stacks;
    }

    let now = time::Instant::now();

    split_values(
        function,
        entry,
        &mut dominator_tree(function.graph(), entry)?,
        &mut FxHashMap::default(),
    );

    let split_values_time = now.elapsed();
    println!("-split values: {:?}", split_values_time);

    let now = time::Instant::now();
    loop {
        let def_use = DefUse::new(function);

        let mut phis_to_remove = Vec::<(NodeId, usize)>::new();
        let mut values_to_replace = HashMap::<ValueId, ValueId>::new();

        for node in function.graph().nodes().clone() {
            let block = function.block(node).unwrap();
            for (phi_index, phi) in block.phi_instructions.iter().cloned().enumerate() {
                let unique = phi
                    .incoming_values
                    .values()
                    .cloned()
                    .collect::<FxHashSet<_>>();
                if unique.len() == 1 {
                    let new_value = *unique.iter().next().unwrap();
                    if new_value != phi.dest {
                        values_to_replace.insert(phi.dest, new_value);
                    }
                    phis_to_remove.push((node, phi_index));
                } else if let Some(def_use_info) = def_use.get(phi.dest) {
                    if def_use_info
                        .reads
                        .iter()
                        .filter(|InstructionLocation(other_node, other_instruction_index)| {
                            if *other_node == node {
                                if let InstructionIndex::Phi(other_phi_index) =
                                    *other_instruction_index
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

        for (node, phi_index) in phis_to_remove.into_iter().rev() {
            function
                .block_mut(node)
                .unwrap()
                .phi_instructions
                .remove(phi_index);
        }

        // we dont need to worry about where to replace since ssa form means values will only be written to once :)
        for node in function.graph().nodes().clone() {
            for (&old, &new) in &values_to_replace {
                let block = function.block_mut(node).unwrap();
                for instruction_index in block.indices() {
                    block
                        .value_info_mut(instruction_index)
                        .unwrap()
                        .replace_values_read(old, new);
                }
            }
        }
    }

    let pruned = now.elapsed();
    println!("-pruning: {:?}", pruned);

    Ok(())
}
