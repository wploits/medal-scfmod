use std::borrow::Cow;
use std::io::Write;

use dot::{GraphWalk, Labeller};

use super::{Edge, Graph, NodeId};

impl<'a> Labeller<'a, NodeId, Edge> for Graph {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("my_graph").unwrap()
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }
}

impl<'a> GraphWalk<'a, NodeId, Edge> for Graph {
    fn nodes(&'a self) -> dot::Nodes<'a, NodeId> {
        Cow::Borrowed(self.nodes())
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        Cow::Borrowed(self.edges())
    }

    fn source(&self, e: &Edge) -> NodeId {
        e.source
    }

    fn target(&self, e: &Edge) -> NodeId {
        e.destination
    }
}

pub fn render_to<W: Write>(graph: &Graph, output: &mut W) -> std::io::Result<()> {
    dot::render(graph, output)
}
