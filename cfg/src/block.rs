use ast::RcLocal;
use enum_as_inner::EnumAsInner;
use petgraph::stable_graph::NodeIndex;

#[derive(Debug, Clone)]
pub struct BasicBlockEdge {
    pub node: NodeIndex,
    pub arguments: Vec<(RcLocal, RcLocal)>,
}

impl BasicBlockEdge {
    fn new(node: NodeIndex) -> Self {
        Self {
            node,
            arguments: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Terminator {
    Jump(BasicBlockEdge),
    Conditional(BasicBlockEdge, BasicBlockEdge),
}

impl Terminator {
    pub fn jump(node: NodeIndex) -> Self {
        Self::Jump(BasicBlockEdge::new(node))
    }

    pub fn conditional(then_node: NodeIndex, else_node: NodeIndex) -> Self {
        Self::Conditional(
            BasicBlockEdge::new(then_node),
            BasicBlockEdge::new(else_node),
        )
    }

    pub fn replace_branch(&mut self, old: NodeIndex, new: NodeIndex) {
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

    pub fn edges(&self) -> Vec<&BasicBlockEdge> {
        match self {
            Terminator::Jump(edge) => vec![edge],
            Terminator::Conditional(then_edge, else_edge) => vec![then_edge, else_edge],
        }
    }

    pub fn edges_mut(&mut self) -> Vec<&mut BasicBlockEdge> {
        match self {
            Terminator::Jump(edge) => vec![edge],
            Terminator::Conditional(then_edge, else_edge) => vec![then_edge, else_edge],
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlock {
    pub ast: ast::Block,
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }

    pub fn terminator_mut(&mut self) -> Option<&mut Terminator> {
        self.terminator.as_mut()
    }
}
