use std::{borrow::Cow, io::Write};

use dot::{GraphWalk, Labeller};

use crate::{Directed, EdgeType, Undirected};

use super::{Edge, Graph, NodeId};

impl<'a> Labeller<'a, NodeId, Edge> for Graph<Directed> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("my_graph").unwrap()
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }
}

impl<'a> Labeller<'a, NodeId, Edge> for Graph<Undirected> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("my_graph").unwrap()
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }

    fn kind(&self) -> dot::Kind {
        dot::Kind::Graph
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

pub fn render_to<'a, W: Write, T: EdgeType>(
    graph: &'a Graph<T>,
    output: &mut W,
) -> std::io::Result<()>
where
    Graph<T>: Labeller<'a, NodeId, (NodeId, NodeId)>,
{
    dot::render(graph, output)
}
