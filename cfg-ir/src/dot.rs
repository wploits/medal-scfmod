use std::borrow::Cow;
use std::io::Write;

use dot::{GraphWalk, Labeller};
use itertools::Itertools;

use graph::{Edge, NodeId};

use crate::function::Function;

impl<'a> Labeller<'a, NodeId, Edge> for Function {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("cfg").unwrap()
    }

    fn node_label<'b>(&'b self, n: &NodeId) -> dot::LabelText<'b> {
        let block = self.block(*n).unwrap();
        let phi_iter = block.phi_instructions.iter().map(|phi| phi.to_string());
        let inner_iter = block
            .inner_instructions
            .iter()
            .map(|inner| inner.to_string());
        let terminator_iter = block
            .terminator()
            .iter()
            .map(|terminator| terminator.to_string());
        let label = phi_iter.chain(inner_iter).chain(terminator_iter).join("\n");
        dot::LabelText::LabelStr(label.into())
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(format!("{}", *n)).unwrap()
    }
}

impl<'a> GraphWalk<'a, NodeId, Edge> for Function {
    fn nodes(&'a self) -> dot::Nodes<'a, NodeId> {
        Cow::Borrowed(self.graph().nodes())
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        Cow::Borrowed(self.graph().edges())
    }

    fn source(&self, e: &Edge) -> NodeId {
        e.source
    }

    fn target(&self, e: &Edge) -> NodeId {
        e.destination
    }
}

pub fn render_to<W: Write>(cfg: &Function, output: &mut W) -> std::io::Result<()> {
    dot::render(cfg, output)
}
