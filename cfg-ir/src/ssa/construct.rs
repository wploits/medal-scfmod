use std::{borrow::BorrowMut, collections::HashMap};

use fxhash::{FxHashMap, FxHashSet};
use graph::{algorithms::dominators::compute_dominance_frontiers, NodeId};

use crate::{builder::Builder, function::Function, value::ValueId, instruction::Phi};

use super::error::Error;

pub fn construct(function: &mut Function) -> Result<(), Error> {
    let mut dominance_frontiers = compute_dominance_frontiers(
        function.graph(),
        function
            .graph()
            .entry()
            .ok_or(Error::Graph(graph::Error::NoEntry))?,
        None,
    )?;
    dominance_frontiers.retain(|_, f| !f.is_empty());
    println!("{:#?}", dominance_frontiers);
    let mut node_to_values_written = FxHashMap::default();
    let mut value_written_to_nodes = FxHashMap::default();
    for &node in dominance_frontiers.keys() {
        let block = &function.blocks[&node];
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

    let value_to_nodes_with_phi = FxHashMap::<ValueId, FxHashSet<NodeId>>::default();
    for (&value, nodes) in &mut value_written_to_nodes {
        while let Some(node) = nodes.pop() {
            let nodes_with_phi = value_to_nodes_with_phi
                .entry(value)
                .or_insert_with(FxHashSet::default)
                .borrow_mut();
            for dominance_frontier_node in dominance_frontiers[&node] {
                if !nodes_with_phi.contains(&dominance_frontier_node) {
                    let mut incoming_values = function
                        .graph()
                        .predecessors(dominance_frontier_node)
                        .map(|p| (p, value))
                        .collect::<FxHashMap<_, _>>();
                    Builder::new(function).block(dominance_frontier_node).push_phi(Phi {
                        dest: value,
                        incoming_values,
                    });
                }
            }
        }
    }

    Ok(())
}
