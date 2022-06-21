pub mod algorithms;

pub mod error;
pub use error::Error;
use fxhash::FxHashSet;
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(feature = "dot")]
pub mod dot;

#[derive(Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub usize);

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "N{}", self.0)
    }
}

impl std::fmt::Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// TODO: this should be a struct with named fields
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Edge {
    pub source: NodeId,
    pub destination: NodeId,
}

impl Edge {
    pub fn new(source: NodeId, destination: NodeId) -> Self {
        Self {
            source,
            destination,
        }
    }
}

impl From<(usize, usize)> for Edge {
    fn from((source, destination): (usize, usize)) -> Self {
        Self::new(NodeId(source), NodeId(destination))
    }
}

impl From<(NodeId, NodeId)> for Edge {
    fn from((source, destination): (NodeId, NodeId)) -> Self {
        Self::new(source, destination)
    }
}

/// A control flow graph
#[derive(Debug, Clone)]
pub struct Graph {
    nodes: Vec<NodeId>,
    edges: Vec<Edge>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn from_edges(edges: Vec<impl Into<Edge>>) -> Self {
        let edges = edges
            .into_iter()
            .map(|edge| edge.into())
            .collect::<Vec<Edge>>();
        let mut nodes = Vec::new();
        for edge in &edges {
            nodes.push(edge.source);
            nodes.push(edge.destination);
        }
        nodes.sort_unstable();
        nodes.dedup();

        Self { nodes, edges }
    }

    pub fn nodes(&self) -> &Vec<NodeId> {
        &self.nodes
    }

    pub fn edges(&self) -> &Vec<Edge> {
        &self.edges
    }

    pub fn node_exists(&self, node: NodeId) -> bool {
        self.nodes.contains(&node)
    }

    pub fn successors(&self, node: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.edges.iter().cloned().filter_map(move |edge| {
            if edge.source == node {
                Some(edge.destination)
            } else {
                None
            }
        })
    }

    pub fn predecessors(&self, node: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.edges.iter().cloned().filter_map(move |edge| {
            if edge.destination == node {
                Some(edge.source)
            } else {
                None
            }
        })
    }

    pub fn add_edge(&mut self, edge: Edge) -> Result<()> {
        if !self.node_exists(edge.source) {
            return Err(Error::InvalidNode(edge.source));
        }
        if !self.node_exists(edge.destination) {
            return Err(Error::InvalidNode(edge.destination));
        }
        self.edges.push(edge);
        Ok(())
    }

    pub fn remove_edge(&mut self, edge: Edge) -> Result<()> {
        if !self.node_exists(edge.source) {
            return Err(Error::InvalidNode(edge.source));
        }
        if !self.node_exists(edge.destination) {
            return Err(Error::InvalidNode(edge.destination));
        }
        self.edges.retain(|other_edge| edge != *other_edge);
        Ok(())
    }

    pub fn add_node(&mut self) -> Result<NodeId> {
        let node = match self.nodes.last() {
            Some(n) => NodeId(n.0 + 1),
            None => NodeId(1),
        };
        self.nodes.push(node);
        Ok(node)
    }

    pub fn add_node_with_id(&mut self, node: NodeId) -> Result<()> {
        if self.node_exists(node) {
            return Err(Error::NodeExists(node));
        }
        self.nodes.push(node);
        Ok(())
    }

    pub fn remove_node(&mut self, node: NodeId) -> Result<()> {
        self.nodes.retain(|other_node| *other_node != node);
        self.edges
            .retain(|e| e.source != node && e.destination != node);
        Ok(())
    }

    pub fn pre_order(&self, root: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(root) {
            return Err(Error::InvalidNode(root));
        }

        let mut visited: FxHashSet<NodeId> = FxHashSet::default();
        let mut stack: Vec<NodeId> = Vec::new();
        let mut order: Vec<NodeId> = Vec::new();

        stack.push(root);

        while let Some(node) = stack.pop() {
            if !visited.insert(node) {
                continue;
            }

            order.push(node);

            for successor in self.successors(node) {
                stack.push(successor);
            }
        }

        Ok(order)
    }

    pub fn post_order(&self, root: NodeId) -> Result<Vec<NodeId>> {
        if !self.node_exists(root) {
            return Err(Error::InvalidNode(root));
        }

        let mut visited: FxHashSet<NodeId> = FxHashSet::default();
        let mut order: Vec<NodeId> = Vec::new();

        // TODO: recursive bad or smthn
        fn dfs_walk(
            graph: &Graph,
            node: NodeId,
            visited: &mut FxHashSet<NodeId>,
            order: &mut Vec<NodeId>,
        ) -> Result<()> {
            visited.insert(node);
            for successor in graph.successors(node) {
                if !visited.contains(&successor) {
                    dfs_walk(graph, successor, visited, order)?;
                }
            }
            order.push(node);
            Ok(())
        }

        dfs_walk(self, root, &mut visited, &mut order)?;

        Ok(order)
    }
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}
