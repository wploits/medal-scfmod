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
        let entry = graph.entry().ok_or(Error::NoEntry)?;
        let mut edges = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = Vec::new();
        stack.push(entry);
        visited.insert(entry);
        while let Some(node) = stack.pop() {
            for &successor in graph.successors(node) {
                if visited.insert(successor) {
                    stack.push(successor);
                } else {
                    println!("found back edge: {} -> {}", node, successor);
                    edges.push(Edge(node, successor));
                }
            }
        }
        Ok(edges)
    }

    fn identify_break_goto(graph: &Graph, headers: &HashSet<NodeId>) -> Result<Vec<Edge>> {
        let entry = graph.entry().ok_or(Error::NoEntry)?;
        let mut edges = Vec::new();
        let preorder = graph.compute_dfs_preorder(entry)?;
        for &node in &preorder {
            let successors = graph.successors(node).collect::<HashSet<_>>();
            if successors.len() >= 2 {
                // a -> b
                // TODO: if a is a header, or there is a path a -> header dom a, and there is no path b -> a, a -> b is a break/goto edge (i think?)
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