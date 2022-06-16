pub mod dominators;

use fxhash::FxHashSet;

use crate::{Edge, Error, Graph, NodeId, Result};

use self::dominators::compute_immediate_dominators;

pub struct BackEdges(Vec<Edge>);

impl IntoIterator for BackEdges {
    type Item = Edge;
    type IntoIter = std::vec::IntoIter<Edge>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

pub fn dfs_tree(graph: &Graph, root: NodeId) -> Result<Graph> {
    if !graph.node_exists(root) {
        return Err(Error::InvalidNode(root));
    }

    let mut tree = Graph::new();
    let mut stack = Vec::new();
    let mut visited = FxHashSet::default();
    visited.insert(root);

    tree.add_node_with_id(root)?;
    for successor in graph.successors(root) {
        stack.push((root, successor));
    }

    while let Some((pred, index)) = stack.pop() {
        if tree.node_exists(index) {
            continue;
        }

        tree.add_node_with_id(index)?;
        tree.add_edge(Edge::new(pred, index))?;

        for successor in graph.successors(index) {
            stack.push((index, successor));
        }
    }

    Ok(tree)
}

pub fn back_edges(graph: &Graph, root: NodeId) -> Result<Vec<Edge>> {
    let mut back_edges = Vec::new();

    for (node, dominators) in dominators::dominators(graph, root, &compute_immediate_dominators(graph, root, &dfs_tree(graph, root)?)?)? {
        for successor in graph.successors(node) {
            if dominators.contains(&successor) {
                back_edges.push(Edge::new(node, successor));
            }
        }
    }

    Ok(back_edges)
}
