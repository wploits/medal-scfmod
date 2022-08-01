use std::ops::{Deref, DerefMut};

use ast::RcLocal;
use enum_as_inner::EnumAsInner;
use fxhash::FxHashMap;
use graph::NodeId;

#[derive(Debug, Clone)]
pub struct BasicBlockEdge {
    pub node: NodeId,
    pub arguments: FxHashMap<RcLocal, RcLocal>,
}

impl BasicBlockEdge {
    fn new(node: NodeId) -> Self {
        Self {
            node,
            arguments: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Terminator {
    Jump(BasicBlockEdge),
    Conditional(BasicBlockEdge, BasicBlockEdge),
}

impl Terminator {
    pub fn jump(node: NodeId) -> Self {
        Self::Jump(BasicBlockEdge::new(node))
    }

    pub fn conditional(then_node: NodeId, else_node: NodeId) -> Self {
        Self::Conditional(
            BasicBlockEdge::new(then_node),
            BasicBlockEdge::new(else_node),
        )
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

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub ast: ast::Block,
    pub terminator: Option<Terminator>,
}

impl Deref for BasicBlock {
    type Target = ast::Block;

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

impl DerefMut for BasicBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ast
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self {
            ast: ast::Block::new(),
            terminator: None,
        }
    }
}

impl BasicBlock {
    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }
}
