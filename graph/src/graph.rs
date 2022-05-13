use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use super::edge::Edge;
use super::error::Error;
use super::{NodeId, Result};

/// A control flow graph
#[derive(Debug, Clone)]
pub struct Graph {
    nodes: Vec<NodeId>,
    edges: Vec<Edge>,
    entry: Option<NodeId>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            entry: None,
        }
    }

    pub fn from_edges(edges: Vec<Edge>) -> Self {
        let mut nodes = Vec::new();
        for edge in &edges {
            nodes.push(edge.0);
            nodes.push(edge.1);
        }
        nodes.sort_unstable();
        nodes.dedup();

        Self {
            nodes,
            edges,
            entry: None,
        }
    }

    pub fn nodes(&self) -> &Vec<NodeId> {
        &self.nodes
    }

    pub fn edges(&self) -> &Vec<Edge> {
        &self.edges
    }

    pub fn entry(&self) -> &Option<NodeId> {
        &self.entry
    }

    pub fn set_entry(&mut self, new_entry: NodeId) -> Result<()> {
        if self.node_exists(new_entry) {
            self.entry = Some(new_entry);
            Ok(())
        } else {
            Err(Error::InvalidNode(new_entry))
        }
    }

    pub fn node_exists(&self, node: NodeId) -> bool {
        self.nodes.contains(&node)
    }

    pub fn successors(&self, node: NodeId) -> impl Iterator<Item = &NodeId> + '_ {
        self.edges.iter().filter_map(
            move |edge| {
                if edge.0 == node {
                    Some(&edge.1)
                } else {
                    None
                }
            },
        )
    }

    pub fn predecessors(&self, node: NodeId) -> impl Iterator<Item = &NodeId> + '_ {
        self.edges.iter().filter_map(
            move |edge| {
                if edge.1 == node {
                    Some(&edge.0)
                } else {
                    None
                }
            },
        )
    }

    pub fn path_exists(&self, from: NodeId, to: NodeId) -> Result<bool> {
        // TODO: check while computing preorder
        Ok(self.compute_dfs_preorder(from)?.contains(&to))
    }

    // same as path_exists but `from' cannot be `to'
    pub fn strict_path_exists(&self, from: NodeId, to: NodeId) -> Result<bool> {
        // TODO: check while computing preorder
        Ok(self
            .compute_dfs_preorder(from)?
            .iter()
            .skip(1)
            .any(|x| *x == to))
    }

    pub fn add_edge(&mut self, edge: Edge) -> Result<()> {
        if !self.node_exists(edge.0) {
            return Err(Error::InvalidNode(edge.0));
        }
        if !self.node_exists(edge.1) {
            return Err(Error::InvalidNode(edge.1));
        }

        self.edges.push(edge);

        Ok(())
    }

    pub fn remove_edge(&mut self, edge: Edge) -> Result<()> {
        if !self.node_exists(edge.0) {
            return Err(Error::InvalidNode(edge.0));
        }
        if !self.node_exists(edge.1) {
            return Err(Error::InvalidNode(edge.1));
        }

        self.edges.retain(|other_edge| edge != *other_edge);

        Ok(())
    }

    pub fn add_node(&mut self) -> Result<NodeId> {
        let node = match self.nodes.last() {
            Some(n) => n + 1,
            None => 1,
        };

        self.nodes.push(node);

        Ok(node)
    }

    pub fn remove_node(&mut self, node: NodeId) -> Result<()> {
        self.nodes.retain(|other_node| *other_node != node);
        self.edges.retain(|e| e.0 != node && e.1 != node);
        Ok(())
    }

    fn compute_bottom_up_dfs_preorder(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode(node));
        }

        let mut visited = HashSet::new();
        let mut stack = Vec::new();
        let mut order = Vec::new();

        visited.insert(node);
        stack.push(node);

        while let Some(node) = stack.pop() {
            order.push(node);

            for &predecessor in self.predecessors(node) {
                if !visited.contains(&predecessor) {
                    visited.insert(predecessor);
                    stack.push(predecessor);
                }
            }
        }

        Ok(order)
    }

    pub fn compute_dfs_preorder(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode(node));
        }

        let mut visited: HashSet<NodeId> = HashSet::new();
        let mut stack: Vec<NodeId> = Vec::new();
        let mut order: Vec<NodeId> = Vec::new();

        stack.push(node);

        while let Some(node) = stack.pop() {
            if !visited.insert(node) {
                continue;
            }

            order.push(node);

            for &successor in self.successors(node) {
                stack.push(successor);
            }
        }

        Ok(order)
    }

    pub fn compute_postorder(&self, node: NodeId) -> Result<(Vec<NodeId>, HashMap<NodeId, HashSet<NodeId>>)> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode(node));
        }

        let mut visited: HashSet<NodeId> = HashSet::default();
        let mut order: Vec<NodeId> = Vec::new();
        let mut predecessor_sets = HashMap::new();

        // TODO: recursive bad or smthn
        fn dfs_walk(
            graph: &Graph,
            node: NodeId,
            visited: &mut HashSet<NodeId>,
            order: &mut Vec<NodeId>,
            predecessor_sets: &mut HashMap<NodeId, HashSet<NodeId>>
        ) -> Result<()> {
            visited.insert(node);
            for &successor in graph.successors(node) {
                predecessor_sets.entry(node).or_insert_with(HashSet::new).insert(successor);
                if !visited.contains(&successor) {
                    dfs_walk(graph, successor, visited, order, predecessor_sets)?;
                }
            }
            order.push(node);
            Ok(())
        }

        dfs_walk(self, node, &mut visited, &mut order, &mut predecessor_sets)?;

        Ok((order, predecessor_sets))
    }

    pub fn compute_bfs_level_order(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode(node));
        }

        let mut visited: HashSet<NodeId> = HashSet::new();
        let mut stack: Vec<NodeId> = Vec::new();
        let mut order: Vec<NodeId> = Vec::new();

        stack.push(node);
        visited.insert(node);
        order.push(node);

        while let Some(node) = stack.pop() {
            for &successor in self.successors(node) {
                if visited.insert(successor) {
                    stack.push(successor);
                    order.push(successor);
                }
            }
        }

        Ok(order)
    }

    pub fn compute_bottom_up_bfs_level_order(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode(node));
        }

        let mut visited: HashSet<NodeId> = HashSet::new();
        let mut stack: Vec<NodeId> = Vec::new();
        let mut order: Vec<NodeId> = Vec::new();

        stack.push(node);
        visited.insert(node);
        order.push(node);

        while let Some(node) = stack.pop() {
            for &predecessor in self.predecessors(node) {
                if visited.insert(predecessor) {
                    stack.push(predecessor);
                    order.push(predecessor);
                }
            }
        }

        Ok(order)
    }

    // thanks petgraph lol
    pub fn compute_immediate_dominators(&self, root: NodeId) -> Result<HashMap<NodeId, NodeId>> {
        let (postorder, predecessor_sets) = self.compute_postorder(root)?;
        
        let node_to_post_order_idx: HashMap<_, _> = postorder
            .iter()
            .enumerate()
            .map(|(idx, &node)| (node, idx))
            .collect();

        let idx_to_predecessor_vec = postorder
            .iter()
            .map(|node| {
                predecessor_sets
                    .remove(node)
                    .map(|predecessors| {
                        predecessors
                            .into_iter()
                            .map(|p| *node_to_post_order_idx.get(&p).unwrap())
                            .collect()
                    })
                    .unwrap_or_else(Vec::new)
            })
            .collect();
    
        let length = postorder.len();

        const UNDEFINED: usize = std::usize::MAX;

        let mut dominators = vec![UNDEFINED; length];
        dominators[length - 1] = length - 1;
        
        Ok(doms)
    }

    fn compute_immediate_postdominators(&self, root: NodeId) -> Result<()> {
        todo!();
    }

    pub fn compute_dominance_frontiers(
        &self,
        root: NodeId,
        immediate_dominators: Option<Cow<HashMap<NodeId, NodeId>>>,
    ) -> Result<HashMap<NodeId, HashSet<NodeId>>> {
        let immediate_dominators = match immediate_dominators {
            Some(immediate_dominators) => immediate_dominators,
            None => Cow::Owned(self.compute_immediate_dominators(root)?),
        };

        if !self.node_exists(root) {
            return Err(Error::InvalidNode(root));
        }

        let mut dominance_frontiers = HashMap::new();

        for &index in &self.nodes {
            dominance_frontiers.insert(index, HashSet::new());
        }

        for &index in &self.nodes {
            let preds = self.predecessors(index).cloned().collect::<Vec<_>>();
            if preds.len() > 1 {
                for pred in preds {
                    let mut runner = pred;
                    let node_idom = *immediate_dominators.get(&index).unwrap();

                    while runner != node_idom {
                        dominance_frontiers.get_mut(&runner).unwrap().insert(index);
                        runner = *immediate_dominators.get(&runner).unwrap();
                    }
                }
            }
        }

        Ok(dominance_frontiers)
    }
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}
