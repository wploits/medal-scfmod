use ast::RcLocal;
use enum_as_inner::EnumAsInner;
use petgraph::stable_graph::NodeIndex;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum BranchType {
    #[default]
    Unconditional,
    Then,
    Else,
}

#[derive(Debug, Clone, Default)]
pub struct BlockEdge {
    pub branch_type: BranchType,
    // TODO: why is this not a hash map?
    pub arguments: Vec<(RcLocal, RcLocal)>,
}

impl BlockEdge {
    pub fn new(branch_type: BranchType) -> Self {
        Self {
            branch_type,
            arguments: Vec::new(),
        }
    }
}
