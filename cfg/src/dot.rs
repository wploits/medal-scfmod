use std::{borrow::Cow, io::Write};

use dot::{GraphWalk, LabelText, Labeller};

use graph::{Edge, NodeId};

use crate::{block::Terminator, function::Function};

impl<'a> Labeller<'a, NodeId, Edge> for Function<'_> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("cfg").unwrap()
    }

    fn node_label<'b>(&'b self, n: &NodeId) -> dot::LabelText<'b> {
        let block = self.block(*n).unwrap();
        dot::LabelText::LabelStr(block.to_string().into())
            .prefix_line(dot::LabelText::LabelStr(n.to_string().into()))
    }

    fn edge_label<'b>(&'b self, e: &Edge) -> dot::LabelText<'b> {
        let terminator = self.block(e.0).unwrap().terminator.as_ref();
        match terminator {
            Some(Terminator::Conditional(then_edge, _)) => {
                if e.1 == then_edge.node {
                    dot::LabelText::LabelStr("t".into())
                } else {
                    dot::LabelText::LabelStr("e".into())
                }
            }
            _ => dot::LabelText::LabelStr("".into()),
        }
    }

    fn node_id(&'a self, n: &NodeId) -> dot::Id<'a> {
        dot::Id::new(n.to_string()).unwrap()
    }

    fn node_shape(&'a self, _n: &NodeId) -> Option<LabelText<'a>> {
        Some(LabelText::LabelStr("rect".into()))
    }
}

impl<'a> GraphWalk<'a, NodeId, Edge> for Function<'_> {
    fn nodes(&'a self) -> dot::Nodes<'a, NodeId> {
        Cow::Borrowed(self.graph().nodes())
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        Cow::Borrowed(self.graph().edges())
    }

    fn source(&self, e: &Edge) -> NodeId {
        e.0
    }

    fn target(&self, e: &Edge) -> NodeId {
        e.1
    }
}

pub fn render_to<W: Write>(cfg: &Function, output: &mut W) -> std::io::Result<()> {
    dot::render(cfg, output)
}
