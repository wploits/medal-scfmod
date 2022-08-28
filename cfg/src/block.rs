use std::ops::{Deref, DerefMut};

use ast::RcLocal;
use enum_as_inner::EnumAsInner;
use fxhash::FxHashMap;
use graph::NodeId;

#[derive(Debug, Clone)]
pub struct BlockEdge {
    pub node: NodeId,
    pub arguments: FxHashMap<RcLocal, RcLocal>,
}

impl BlockEdge {
    fn new(node: NodeId) -> Self {
        Self {
            node,
            arguments: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Edges {
    Jump(BlockEdge),
    Conditional(BlockEdge, BlockEdge),
}

impl Edges {
    pub fn jump(node: NodeId) -> Self {
        Self::Jump(BlockEdge::new(node))
    }

    pub fn conditional(then_node: NodeId, else_node: NodeId) -> Self {
        Self::Conditional(BlockEdge::new(then_node), BlockEdge::new(else_node))
    }

    pub fn replace_branch(&mut self, old: NodeId, new: NodeId) {
        match self {
            Self::Jump(edge) => {
                assert!(edge.node == old);
                edge.node = new;
            }
            Self::Conditional(then_edge, else_edge) => {
                if then_edge.node == old {
                    then_edge.node = new;
                } else if else_edge.node == old {
                    else_edge.node = new;
                }
            }
        }
    }

    pub fn swap_edges(&mut self) {
        if let Self::Conditional(then_edge, else_edge) = self {
            std::mem::swap(then_edge, else_edge);
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlock {
    pub ast: ast::Block,
    pub terminator: Option<Edges>,
}

impl BasicBlock {
    pub fn terminator(&self) -> &Option<Edges> {
        &self.terminator
    }
}
