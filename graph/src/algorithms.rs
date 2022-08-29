pub mod dag;
pub mod dominators;

use contracts::requires;
use fxhash::FxHashSet;

use crate::{Directed, Edge, Graph, NodeId, Result, Undirected};

use self::dominators::compute_immediate_dominators;

pub struct BackEdges(Vec<Edge>);

impl IntoIterator for BackEdges {
    type Item = Edge;
    type IntoIter = std::vec::IntoIter<Edge>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

// if root is not provided, the entire graph is searched
#[requires(root.is_some() -> graph.has_node(root.unwrap()))]
pub fn dfs_tree(graph: &Graph<Directed>, root: Option<NodeId>) -> Graph<Directed> {
    let mut tree = Graph::<Directed>::new();
    let mut stack = Vec::new();

    let nodes = if let Some(root) = root {
        vec![root]
    } else {
        // TODO: test
        graph.nodes.clone()
    };

    for node in nodes {
        if !tree.has_node(node) {
            tree.add_node_with_id(node);
            for successor in graph.successors(node) {
                stack.push((node, successor));
            }
            while let Some((pred, index)) = stack.pop() {
                if tree.has_node(index) {
                    continue;
                }

                tree.add_node_with_id(index);
                tree.add_edge((pred, index));

                for successor in graph.successors(index) {
                    stack.push((index, successor));
                }
            }
        }
    }

    tree
}

#[requires(graph.has_node(root))]
pub fn back_edges(graph: &Graph<Directed>, root: NodeId) -> Result<Vec<Edge>> {
    let mut back_edges = Vec::new();

    for (node, dominators) in dominators::dominators(
        graph,
        root,
        &compute_immediate_dominators(graph, root, &dfs_tree(graph, Some(root))),
    )? {
        for successor in graph.successors(node) {
            if dominators.contains(&successor) {
                back_edges.push((node, successor));
            }
        }
    }

    Ok(back_edges)
}

pub fn connected_components(graph: &Graph<Undirected>) -> Vec<Vec<NodeId>> {
    let mut res = Vec::new();
    let mut seen = FxHashSet::<NodeId>::default();

    for &node in graph.nodes() {
        if !seen.contains(&node) {
            fn bfs(graph: &Graph<Undirected>, node: NodeId, seen: &mut FxHashSet<NodeId>) {
                seen.insert(node);
                for neighbor in graph.neighbors(node) {
                    if !seen.contains(&neighbor) {
                        bfs(graph, neighbor, seen);
                    }
                }
            }
            let mut component = FxHashSet::default();
            bfs(graph, node, &mut component);
            seen.extend(component.iter());
            res.push(component.into_iter().collect());
        }
    }

    res
}
