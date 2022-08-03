use std::{borrow::Cow, io::Write};

use dot::{GraphWalk, Labeller};

use crate::EdgeType;

use super::{Edge, Graph, NodeId};

impl<'a, T: EdgeType> Labeller<'a, NodeId, Edge> for Graph<T> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("my_graph").unwrap()
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }
}

impl<'a, T: EdgeType> GraphWalk<'a, NodeId, Edge> for Graph<T> {
    fn nodes(&'a self) -> dot::Nodes<'a, NodeId> {
        Cow::Borrowed(self.nodes())
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        Cow::Borrowed(self.edges())
    }

    fn source(&self, e: &Edge) -> NodeId {
        e.0
    }

    fn target(&self, e: &Edge) -> NodeId {
        e.1
    }
}

pub fn render_to<W: Write, T: EdgeType>(graph: &Graph<T>, output: &mut W) -> std::io::Result<()> {
    dot::render(graph, output)
}
