use ast::RcLocal;
use fxhash::FxHashMap;
use graph::NodeId;

pub struct BasicBlockEdge<'a> {
    node: NodeId,
    arguments: FxHashMap<RcLocal<'a>, RcLocal<'a>>,
}

#[derive(Debug, Clone)]
pub enum Terminator<'a> {
    Jump(BasicBlockEdge<'a>),
    Conditional(BasicBlockEdge<'a>, BasicBlockEdge<'a>),
    Return,
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    pub block: ast::Block<'a>,
    pub terminator: Option<Terminator<'a>>,
}

impl Default for BasicBlock<'_> {
    fn default() -> Self {
        Self {
            block: ast::Block::new(),
            terminator: None,
        }
    }
}

impl<'a> BasicBlock<'a> {
    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }
}
