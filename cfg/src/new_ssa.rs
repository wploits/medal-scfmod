use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};
use graph::algorithms::{
    dfs_tree,
    dominators::{compute_dominance_frontiers, compute_immediate_dominators, dominator_tree},
};

use crate::{
    function::Function,
    instruction::{location::InstructionIndex, value_info::ValueInfo, Phi},
    new_def_use,
    value::ValueId,
};

pub fn construct(function: &mut Function) {
    let entry = function.entry().unwrap();
    let graph = function.graph();

    let dfs = dfs_tree(graph, entry).unwrap();
    let idoms = compute_immediate_dominators(graph, entry, &dfs).unwrap();
    let dom_tree = dominator_tree(graph, &idoms).unwrap();

    let mut dominance_frontiers = compute_dominance_frontiers(graph, entry, &idoms, &dfs).unwrap();
    dominance_frontiers.retain(|_, f| !f.is_empty());

    for (node, dominance_frontier_nodes) in dominance_frontiers {
        let block = function.block(node).unwrap();
        let writes = block
            .inner_instructions
            .iter()
            .flat_map(|inner| inner.values_written())
            .chain(
                block
                    .terminator()
                    .as_ref()
                    .unwrap()
                    .values_written()
                    .into_iter(),
            )
            .collect::<Vec<_>>();

        for value_written in writes {
            for &dominance_frontier_node in &dominance_frontier_nodes {
                let incoming_values = function
                    .graph()
                    .predecessors(dominance_frontier_node)
                    .map(|predecessor| (predecessor, value_written))
                    .collect();
                function
                    .block_mut(dominance_frontier_node)
                    .unwrap()
                    .phi_instructions
                    .push(Phi {
                        dest: value_written,
                        incoming_values,
                    })
            }
        }
    }

    let mut stack = vec![(
        entry,
        Rc::new(FxHashMap::<ValueId, Vec<ValueId>>::default()),
    )];
    let mut visited = FxHashSet::default();
    while let Some((node, mut value_stacks)) = stack.pop() {
        if visited.insert(node) {
            let block = function.block_mut(node).unwrap();
            let block_reads = block
                .inner_instructions
                .iter()
                .enumerate()
                .map(|(index, inner)| (InstructionIndex::Inner(index), inner.values_read()))
                .chain(std::iter::once((
                    InstructionIndex::Terminator,
                    block.terminator().as_ref().unwrap().values_read(),
                )))
                .collect::<Vec<_>>();

            for (index, values_read) in block_reads {
                for read_value in values_read {
                    if let Some(value_stack) = value_stacks.get(&read_value) {
                        block.replace_values_read(index, read_value, *value_stack.last().unwrap());
                    }
                }
            }

            

            let def_use = new_def_use::DefUse::capture(function);
            for (value, value_def_use) in def_use
                .access_by_node(node)
                .into_iter()
                .filter(|(_, value_def_use)| !value_def_use.writes.is_empty())
            {
                for write_location in value_def_use
                    .writes
                    .iter()
                    .filter(|location| location.node == node)
                {
                    if value_stacks.contains_key(&value) {
                        let new_value = function.value_allocator.borrow_mut().new_value();
                        Rc::make_mut(&mut value_stacks)
                            .get_mut(&value)
                            .unwrap()
                            .push(new_value);
                        function.block_mut(node).unwrap().replace_values_written(
                            write_location.index,
                            value,
                            new_value,
                        );
                    } else {
                        Rc::make_mut(&mut value_stacks).insert(value, vec![value]);
                    }
                }
            }

            for successor in function.graph().successors(node).collect::<Vec<_>>() {
                for phi in &mut function.block_mut(successor).unwrap().phi_instructions {
                    let old_value = phi.incoming_values[&node];
                    if let Some(value_stack) = value_stacks.get(&old_value) {
                        phi.incoming_values
                            .insert(node, *value_stack.last().unwrap());
                    }
                }
            }
        }

        for child_block in dom_tree.successors(node) {
            if !visited.contains(&child_block) {
                stack.push((node, value_stacks.clone()));
                stack.push((child_block, value_stacks));
                break;
            }
        }
    }
}
