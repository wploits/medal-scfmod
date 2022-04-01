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
    immediate_dominators: HashMap<NodeId, NodeId>,
    immediate_postdominators: HashMap<NodeId, NodeId>,
    dominance_frontiers: HashMap<NodeId, HashSet<NodeId>>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            entry: None,
            immediate_dominators: HashMap::new(),
            immediate_postdominators: HashMap::new(),
            dominance_frontiers: HashMap::new(),
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
            immediate_dominators: HashMap::new(),
            immediate_postdominators: HashMap::new(),
            dominance_frontiers: HashMap::new(),
        }
    }

    pub fn nodes(&self) -> &Vec<NodeId> {
        &self.nodes
    }

    pub fn edges(&self) -> &Vec<Edge> {
        &self.edges
    }

    pub fn entry(&self) -> Option<&NodeId> {
        self.entry.as_ref()
    }

    pub fn set_entry(&mut self, new_entry: NodeId) -> Result<()> {
        if self.node_exists(new_entry) {
            self.entry = Some(new_entry);
            self.update_control_flow_info()?;
            Ok(())
        } else {
            Err(Error::InvalidNode { node: new_entry })
        }
    }

    pub fn immediate_dominators(&self) -> &HashMap<NodeId, NodeId> {
        &self.immediate_dominators
    }

    pub fn immediate_postdominators(&self) -> &HashMap<NodeId, NodeId> {
        &self.immediate_postdominators
    }

    pub fn dominance_frontiers(&self) -> &HashMap<NodeId, HashSet<NodeId>> {
        &self.dominance_frontiers
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

    pub fn immediately_dominates(&self, dom: NodeId, node: NodeId) -> Result<bool> {
        if self.entry.is_none() {
            return Err(Error::NoEntry);
        }
        if !self.node_exists(node) {
            return Err(Error::InvalidNode { node });
        }
        if !self.node_exists(dom) {
            return Err(Error::InvalidNode { node: dom });
        }

        Ok(*self.immediate_dominators.get(&node).unwrap() == dom)
    }

    pub fn dominates(&self, dom: NodeId, node: NodeId) -> Result<bool> {
        if self.entry.is_none() {
            return Err(Error::NoEntry);
        }
        if !self.node_exists(node) {
            return Err(Error::InvalidNode { node });
        }
        if !self.node_exists(dom) {
            return Err(Error::InvalidNode { node: dom });
        }

        let mut runner = node;
        loop {
            if runner == dom {
                return Ok(true);
            }
            if let Some(idom) = self.immediate_dominators.get(&runner) {
                runner = *idom;
            } else {
                return Ok(false);
            }
        }
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
            return Err(Error::InvalidNode { node: edge.0 });
        }
        if !self.node_exists(edge.1) {
            return Err(Error::InvalidNode { node: edge.1 });
        }

        self.edges.push(edge);
        self.update_control_flow_info()?;

        Ok(())
    }

    pub fn remove_edge(&mut self, edge: Edge) -> Result<()> {
        if !self.node_exists(edge.0) {
            return Err(Error::InvalidNode { node: edge.0 });
        }
        if !self.node_exists(edge.1) {
            return Err(Error::InvalidNode { node: edge.1 });
        }

        self.edges.retain(|other_edge| edge != *other_edge);
        self.update_control_flow_info()?;

        Ok(())
    }

    pub fn add_node(&mut self) -> Result<NodeId> {
        let node = match self.nodes.last() {
            Some(n) => n + 1,
            None => 1,
        };

        self.nodes.push(node);
        self.update_control_flow_info()?;

        Ok(node)
    }

    pub fn remove_node(&mut self, node: NodeId) -> Result<()> {
        self.nodes.retain(|other_node| *other_node != node);
        self.edges.retain(|e| e.0 != node && e.1 != node);
        self.update_control_flow_info()?;

        Ok(())
    }

    //TODO: this is really slow
    fn update_control_flow_info(&mut self) -> Result<()> {
        if let Some(entry) = self.entry {
            //self.compute_immediate_dominators(entry)?;
            //self.compute_immediate_postdominators(entry)?;
            //self.compute_dominance_frontiers(entry)?;
        }
        Ok(())
    }

    fn compute_bottom_up_dfs_preorder(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode { node });
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
            return Err(Error::InvalidNode { node });
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

    pub fn compute_dfs_postorder(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode { node });
        }

        let mut visited: HashSet<NodeId> = HashSet::default();
        let mut order: Vec<NodeId> = Vec::new();

        // TODO: recursive bad or smthn
        fn dfs_walk(
            graph: &Graph,
            node: NodeId,
            visited: &mut HashSet<NodeId>,
            order: &mut Vec<NodeId>,
        ) -> Result<()> {
            visited.insert(node);
            for successor in graph.successors(node) {
                if !visited.contains(successor) {
                    dfs_walk(graph, *successor, visited, order)?;
                }
            }
            order.push(node);
            Ok(())
        }

        dfs_walk(self, node, &mut visited, &mut order)?;

        Ok(order)
    }

    pub fn compute_bfs_level_order(&self, node: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(node) {
            return Err(Error::InvalidNode { node });
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
            return Err(Error::InvalidNode { node });
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

    // see figure 3 from A Simple, Fast Dominance Algorithm (Cooper, et al.) https://www.cs.rice.edu/~keith/EMBED/dom.pdf
    fn compute_immediate_dominators(&mut self, root: NodeId) -> Result<()> {
        self.immediate_dominators.clear();
        let postorder = self.compute_dfs_postorder(root)?;
        for &node in &postorder {
            self.immediate_dominators.insert(node, node);
        }
        let mut changed = true;
        while changed {
            changed = false;

            for node in postorder.iter().skip(1).rev() {
                let predecessors = self.predecessors(*node).cloned().collect::<Vec<_>>();
                if !predecessors.is_empty() {
                    let mut new_idom = *predecessors.get(0).unwrap();
                    for other_pred in predecessors.iter().skip(1) {
                        if self.immediate_dominators.get(other_pred).is_some() {
                            new_idom =
                                intersect(&mut self.immediate_dominators, *other_pred, new_idom);
                        }
                    }
                    if let Some(&b) = self.immediate_dominators.get(node) {
                        if b != new_idom {
                            self.immediate_dominators.insert(*node, new_idom);
                            changed = true;
                        }
                    }
                }
            }
        }

        fn intersect(
            dominators: &mut HashMap<NodeId, NodeId>,
            node1: NodeId,
            node2: NodeId,
        ) -> NodeId {
            let mut finger1 = node1;
            let mut finger2 = node2;
            while finger1 != finger2 {
                while finger1 < finger2 {
                    finger1 = dominators[&finger1];
                }
                while finger2 < finger1 {
                    finger2 = dominators[&finger2];
                }
            }
            finger1
        }

        Ok(())
    }

    fn compute_immediate_postdominators(&self, root: NodeId) -> Result<()> {
        // TODO: finish
        todo!();
        /*let preorder = self.compute_bottom_up_dfs_preorder(0)?;

        let dfs_number: HashMap<NodeId, NodeId> = preorder
            .iter()
            .enumerate()
            .map(|(number, node)| (*node, number))
            .collect();

        let graph_number = &preorder;

        let mut ancestor: HashMap<NodeId, Option<NodeId>> = HashMap::new();
        let mut label: HashMap<NodeId, NodeId> = HashMap::new();
        for (&node, _) in self.vertices.iter() {
            ancestor.insert(node, None);
            label.insert(node, dfs_number[&node]);
        }

        let mut semi = HashMap::new();
        for &node in preorder.iter().skip(1).rev() {
            let mut min_semi = std::NodeId::MAX;

            for &pred in &self.successor_map[&node] {
                if ancestor[&pred].is_some() {
                    compress(&mut ancestor, &mut label, pred);
                }
                min_semi = std::cmp::min(min_semi, label[&pred]);
            }

            semi.insert(node, min_semi);
            label.insert(node, min_semi);

            ancestor.insert(node, dfs_parent(node));
        }
        let semi = semi;

        fn compress(
            ancestor: &mut HashMap<NodeId, Option<NodeId>>,
            label: &mut HashMap<NodeId, NodeId>,
            v: NodeId,
        ) {
            let u = ancestor[&v].unwrap();
            if ancestor[&u].is_some() {
                compress(ancestor, label, u);
                if label[&u] < label[&v] {
                    label.insert(v, label[&u]);
                }
                ancestor.insert(v, ancestor[&u]);
            }
        }

        let mut idoms = HashMap::new();
        for &node in preorder.iter().skip(1) {
            let mut idom = dfs_number[&dfs_parent(node).unwrap()];
            while idom > semi[&node] {
                idom = idoms[&idom];
            }
            idoms.insert(dfs_number[&node], idom);
        }
        let idoms = idoms;

        let mut graph_idoms = HashMap::new();
        for (node, idom) in idoms {
            graph_idoms.insert(graph_number[node], graph_number[idom]);
        }

        Ok(graph_idoms)*/
    }

    fn compute_dominance_frontiers(&mut self, root: NodeId) -> Result<()> {
        if !self.node_exists(root) {
            return Err(Error::InvalidNode { node: root });
        }
        self.dominance_frontiers.clear();

        for &index in &self.nodes {
            self.dominance_frontiers.insert(index, HashSet::new());
        }

        for &index in &self.nodes {
            let preds = self.predecessors(index).cloned().collect::<Vec<_>>();
            if preds.len() > 1 {
                for pred in preds {
                    let mut runner = pred;
                    let node_idom = *self.immediate_dominators.get(&index).unwrap();

                    while runner != node_idom {
                        self.dominance_frontiers
                            .get_mut(&runner)
                            .unwrap()
                            .insert(index);
                        runner = *self.immediate_dominators.get(&runner).unwrap();
                    }
                }
            }
        }

        Ok(())
    }
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}
