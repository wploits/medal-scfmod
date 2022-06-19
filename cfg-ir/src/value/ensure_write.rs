use graph::{
    algorithms::{
        back_edges, dfs_tree,
        dominators::{common_dominator, compute_immediate_dominators, dominators},
    },
    Edge,
};
use itertools::Itertools;

use crate::{
    constant::Constant,
    def_use::DefUse,
    function::Function,
    instruction::{branch_info::BranchInfo, LoadConstant, UnconditionalJump},
};

#[derive(Debug)]
enum AccessType {
    Read,
    Write,
}

pub fn ensure_writes(function: &mut Function) {
    let mut entry = function.entry().unwrap();

    let graph = function.graph();
    let dfs = dfs_tree(graph, entry).unwrap();
    let idoms = compute_immediate_dominators(graph, entry, &dfs).unwrap();
    let dominators = dominators(graph, entry, &idoms).unwrap();
    let back_edges = back_edges(graph, entry).unwrap();

    let def_use = DefUse::new(function);
    for value in def_use.values().collect::<Vec<_>>() {
        let value_def_use = def_use.get(value).unwrap();

        let all_nodes = value_def_use
            .reads
            .iter()
            .chain(value_def_use.writes.iter())
            .map(|location| location.node)
            .unique()
            .collect::<Vec<_>>();

        let should_write_node = common_dominator(&dominators, all_nodes).unwrap();

        let node_first_usage = value_def_use
            .reads
            .iter()
            .map(|location| (location, AccessType::Read))
            .chain(
                value_def_use
                    .writes
                    .iter()
                    .map(|location| (location, AccessType::Write)),
            )
            .find(|(location, _)| location.node == should_write_node);

        if let Some((_first_usage_location, _access_type)) = node_first_usage {
        } else {
            let new_node = function.new_block().unwrap();
            function
                .block_mut(new_node)
                .unwrap()
                .inner_instructions
                .push(
                    LoadConstant {
                        dest: value,
                        constant: Constant::Nil,
                    }
                    .into(),
                );
            for predecessor in function
                .graph()
                .predecessors(should_write_node)
                .collect::<Vec<_>>()
            {
                if !back_edges.contains(&Edge::new(predecessor, should_write_node)) {
                    let mut terminator = function
                        .block_mut(predecessor)
                        .unwrap()
                        .terminator
                        .take()
                        .unwrap();
                    terminator.replace_branch(should_write_node, new_node);
                    function
                        .set_block_terminator(predecessor, Some(terminator))
                        .unwrap();
                }
            }
            function
                .set_block_terminator(new_node, Some(UnconditionalJump(should_write_node).into()))
                .unwrap();
            if entry == should_write_node {
                function.set_entry(new_node).unwrap();
                entry = new_node;
            }
        }
    }
}
