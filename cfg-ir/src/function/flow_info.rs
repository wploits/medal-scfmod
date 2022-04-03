use std::collections::{HashMap, HashSet};
use graph::{NodeId, Graph, edge::Edge, error::Error};
use crate::error::Result;

#[derive(Debug)]
pub enum EdgeType {
    Loop, // break, continue
    Goto,
}

#[derive(Debug)]
pub struct Loop {
    pub header: NodeId,
    pub edges: HashSet<Edge>,
}

#[derive(Debug)]
pub struct FlowInfo {
    pub edges: HashMap<Edge, EdgeType>,
    pub loops: HashMap<NodeId, Loop>,
}

impl FlowInfo {
    // modified dfs algorithm
    fn identify_loops(graph: &Graph) -> Result<Vec<Edge>> {
        fn dfs_visit(graph: &Graph, node: NodeId, visited: &mut HashSet<NodeId>, finished: &mut HashSet<NodeId>, back_edges: &mut Vec<Edge>) {
            visited.insert(node);
            for successor in graph.successors(node) {
                if visited.contains(successor) {
                    println!("back edge detected {} -> {}", node, *successor);
                    back_edges.push(Edge(node, *successor));
                } else {
                    if !finished.contains(successor) {
                        dfs_visit(graph, *successor, visited, finished, back_edges);
                    }
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
                // a -> b
                // TODO: if a is a header, or there is a path a -> header dom a, and there is no path b -> a, a -> b is a break
                for &destination in successors {
                    let mut destination_reachable_headers = graph.compute_dfs_preorder(destination)?;
                    destination_reachable_headers.retain(|n| headers.contains(n));
                    if reachable_headers != destination_reachable_headers {
                        println!("we broke out of something");
                    }
                }
            }
            
        }

        Ok(edges)
    }

    pub fn new(graph: &mut Graph) -> Result<Self> {
        let loops = Self::identify_loops(graph)?;
        let headers = loops.iter().map(|&edge| edge.1).collect::<HashSet<_>>();
        let breaks = Self::identify_break_goto(graph, &headers)?;
        println!("{:?}", breaks);
        Ok(Self {
            edges: HashMap::new(),
            loops: HashMap::new(),
        }) 
    }
}