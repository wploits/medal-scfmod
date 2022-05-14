pub mod dominators;
pub mod flow;

use crate::{Edge, Error, Graph, NodeId, Result};

pub struct BackEdges(Vec<Edge>);

impl IntoIterator for BackEdges {
    type Item = Edge;
    type IntoIter = std::vec::IntoIter<Edge>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub fn dfs_tree(graph: &Graph, root: NodeId) -> Result<(Graph, BackEdges)> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let mut tree = Graph::new();
    let mut stack = Vec::new();
    let mut back_edges = Vec::new();

    tree.add_node_with_id(root)?;
    for &successor in graph.successors(root) {
        stack.push((root, successor));
    }

    while let Some((pred, index)) = stack.pop() {
        if tree.node_exists(index) {
            back_edges.push(Edge::new(pred, index));
            continue;
        }

        tree.add_node_with_id(index)?;
        tree.add_edge(Edge::new(pred, index))?;

        for &successor in graph.successors(index) {
            stack.push((index, successor));
        }
    }

    Ok((tree, BackEdges(back_edges)))
}
