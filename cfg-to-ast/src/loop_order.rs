use graph::algorithms::dfs_tree;
use graph::{Error, Graph, NodeId, Result};

pub(crate) fn loop_lift_order(graph: &Graph) -> Result<Vec<NodeId>> {
    let (dfs_tree, back_edges) = dfs_tree(graph, graph.entry().ok_or(Error::NoEntry)?).unwrap();
    let loop_headers = back_edges
        .into_iter()
        .map(|edge| edge.destination())
        .collect::<Vec<_>>();
    Ok(dfs_tree
        .post_order()?
        .iter()
        .cloned()
        .filter(|node| loop_headers.contains(node))
        .collect::<Vec<_>>())
}
