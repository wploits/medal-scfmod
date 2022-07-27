use contracts::requires;
use fxhash::FxHashSet;

pub mod algorithms;

pub mod error;
pub use error::Error;
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(feature = "dot")]
pub mod dot;

#[derive(Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct NodeId(usize);

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

pub type Edge = (NodeId, NodeId);

/// A directed graph for use in the CFG.
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
            nodes.push(edge.0);
            nodes.push(edge.1);
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

    pub fn has_node(&self, node: NodeId) -> bool {
        self.nodes.contains(&node)
    }

    pub fn has_edge(&self, edge: &Edge) -> bool {
        self.edges.contains(edge)
    }

    #[requires(self.has_node(node))]
    pub fn successors(&self, node: NodeId) -> Vec<NodeId> {
        self.edges
            .iter()
            .filter_map(|edge| if edge.0 == node { Some(edge.1) } else { None })
            .collect()
    }

    #[requires(self.has_node(node))]
    pub fn predecessors(&self, node: NodeId) -> Vec<NodeId> {
        self.edges
            .iter()
            .filter_map(|edge| if edge.1 == node { Some(edge.0) } else { None })
            .collect()
    }

    #[requires(self.has_node(edge.0))]
    #[requires(self.has_node(edge.1))]
    #[requires(!self.has_edge(&edge))]
    pub fn add_edge(&mut self, edge: Edge) {
        self.edges.push(edge);
    }

    #[requires(self.edges.contains(&edge))]
    pub fn remove_edge(&mut self, edge: &Edge) {
        self.edges.retain(|other_edge| edge != other_edge);
    }

    pub fn add_node(&mut self) -> NodeId {
        let node = match self.nodes.last() {
            Some(n) => NodeId(n.0 + 1),
            None => NodeId(1),
        };
        self.nodes.push(node);
        node
    }

    #[requires(!self.has_node(node))]
    pub fn add_node_with_id(&mut self, node: NodeId) {
        self.nodes.push(node);
    }

    #[requires(self.has_node(node))]
    pub fn remove_node(&mut self, node: NodeId) {
        self.nodes.retain(|other_node| *other_node != node);
        self.edges.retain(|e| e.0 != node && e.1 != node);
    }

    #[requires(self.has_node(root))]
    pub fn pre_order(&self, root: NodeId) -> Vec<NodeId> {
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
        order
    }

    #[requires(self.has_node(root))]
    pub fn post_order(&self, root: NodeId) -> Vec<NodeId> {
        let mut visited: FxHashSet<NodeId> = FxHashSet::default();
        let mut order: Vec<NodeId> = Vec::new();
        // TODO: recursive bad or smthn
        fn dfs_walk(
            graph: &Graph,
            node: NodeId,
            visited: &mut FxHashSet<NodeId>,
            order: &mut Vec<NodeId>,
        ) {
            visited.insert(node);
            for successor in graph.successors(node) {
                if !visited.contains(&successor) {
                    dfs_walk(graph, successor, visited, order);
                }
            }
            order.push(node);
        }
        dfs_walk(self, root, &mut visited, &mut order);
        order
    }
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}
