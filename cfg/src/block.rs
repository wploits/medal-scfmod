use std::ops::{Deref, DerefMut};

use ast::RcLocal;
use enum_as_inner::EnumAsInner;
use fxhash::FxHashMap;
use graph::NodeId;

#[derive(Debug, Clone)]
pub struct BasicBlockEdge<'a> {
    pub node: NodeId,
    pub arguments: FxHashMap<RcLocal<'a>, RcLocal<'a>>,
}

impl BasicBlockEdge<'_> {
    fn new(node: NodeId) -> Self {
        Self {
            node,
            arguments: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Terminator<'a> {
    Jump(BasicBlockEdge<'a>),
    Conditional(BasicBlockEdge<'a>, BasicBlockEdge<'a>),
}

impl Terminator<'_> {
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
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    pub ast: ast::Block<'a>,
    pub terminator: Option<Terminator<'a>>,
}

impl<'a> Deref for BasicBlock<'a> {
    type Target = ast::Block<'a>;

    fn deref(&self) -> &Self::Target {
        &self.ast
    }
}

impl<'a> DerefMut for BasicBlock<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ast
    }
}

impl Default for BasicBlock<'_> {
    fn default() -> Self {
        Self {
            ast: ast::Block::new(),
            terminator: None,
        }
    }
}

impl<'a> BasicBlock<'a> {
    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }
}
