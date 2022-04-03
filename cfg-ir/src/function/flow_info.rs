use crate::error::Result;
use graph::{edge::Edge, error::Error, Graph, NodeId};
use std::collections::HashSet;

#[derive(Debug)]
pub struct FlowInfo {}

impl FlowInfo {
    // modified dfs algorithm
    fn identify_loops(graph: &Graph) -> Result<Vec<Edge>> {
        fn dfs_visit(
            graph: &Graph,
            node: NodeId,
            visited: &mut HashSet<NodeId>,
            finished: &mut HashSet<NodeId>,
            back_edges: &mut Vec<Edge>,
        ) {
            visited.insert(node);
            for successor in graph.successors(node) {
                if visited.contains(successor) {
                    back_edges.push(Edge(node, *successor));
                } else if !finished.contains(successor) {
                    dfs_visit(graph, *successor, visited, finished, back_edges);
                }
            }

            visited.remove(&node);
            finished.insert(node);
        }

        let mut visited = HashSet::new();
        let mut finished = HashSet::new();
        let mut back_edges = Vec::new();

        for node in graph.nodes() {
            if !visited.contains(node) && !finished.contains(node) {
                dfs_visit(graph, *node, &mut visited, &mut finished, &mut back_edges);
            }
        }
        Ok(back_edges)
    }

    fn identify_break_goto(graph: &Graph, headers: &HashSet<NodeId>) -> Result<Vec<Edge>> {
        let entry = graph.entry().ok_or(Error::NoEntry)?;
        let mut edges = Vec::new();
        let preorder = graph.compute_dfs_preorder(entry)?;
        for &node in &preorder {
            let successors = graph.successors(node).collect::<HashSet<_>>();
            if successors.len() >= 2 {
                let mut reachable_headers = graph.compute_dfs_preorder(node)?;
                reachable_headers.retain(|n| headers.contains(n));
                for &destination in successors {
                    let mut destination_reachable_headers =
                        graph.compute_dfs_preorder(destination)?;
                    destination_reachable_headers.retain(|n| headers.contains(n));
                    if reachable_headers != destination_reachable_headers {
                        edges.push(Edge(node, destination));
                    }
                }
            }
        }
        Ok(edges)
    }

    pub fn new(graph: &mut Graph) -> Result<Self> {
        let loops = Self::identify_loops(graph)?;
        let headers = loops.iter().map(|edge| edge.1).collect::<HashSet<_>>();
        let breaks = Self::identify_break_goto(graph, &headers)?;
        println!("headers: {:?}", headers);
        println!("continues: {:?}", loops);
        println!("breaks: {:?}", breaks);
        Ok(Self {})
    }
}
