use std::collections::HashSet;

use cfg_ir::function::Function;
use graph::algorithms::{back_edges, dfs_tree};
use graph::{Graph, NodeId};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("No entry")]
    NoEntry,
}

pub type Result<T> = std::result::Result<T, Error>;

fn breaks_out(graph: &Graph, succ: NodeId, header: NodeId, exclude: HashSet<NodeId>) -> bool {
    let mut visited = exclude;
    let mut stack = vec![succ];
    while let Some(node) = stack.pop() {
        if !visited.insert(node) {
            continue;
        }
        if node == header {
            return false;
        }
        for &successor in graph.successors(node) {
            stack.push(successor);
        }
    }
    true
}

fn lift_block(
    cfg: &Graph,
    dfs_tree: &Graph,
    node: NodeId,
    loop_headers: &Vec<NodeId>,
    mut loop_stack: Vec<NodeId>,
) {
    if loop_headers.contains(&node) {
        loop_stack.push(node);
    }

    println!("{} loop stack: {:?}", node, loop_stack);

    let successors = dfs_tree.successors(node).cloned().collect::<Vec<_>>();
    if successors.len() == 1 {
        return lift_block(cfg, dfs_tree, successors[0], loop_headers, loop_stack);
    }

    for succ in successors {
        let new_loop_stack = loop_stack
            .iter()
            .enumerate()
            .position(|(i, &header)| {
                breaks_out(
                    cfg,
                    succ,
                    header,
                    loop_stack.iter().take(i).cloned().collect(),
                )
            })
            .map(|loop_index| loop_stack.iter().take(loop_index).cloned().collect());
        lift_block(
            cfg,
            dfs_tree,
            succ,
            loop_headers,
            new_loop_stack.unwrap_or_else(|| loop_stack.clone()),
        );
    }
}

pub fn lift(cfg: &Function) -> Result<()> {
    let graph = cfg.graph();
    let entry = graph.entry().ok_or(Error::NoEntry)?;

    let dfs_tree = dfs_tree(graph, entry).unwrap();

    let back_edges = back_edges(graph).unwrap();
    let loop_headers = back_edges
        .into_iter()
        .map(|edge| edge.destination())
        .collect::<Vec<_>>();

    lift_block(graph, &dfs_tree, entry, &loop_headers, Vec::new());

    Ok(())
}
