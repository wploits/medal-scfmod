use graph::NodeId;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Hash)]
pub enum InstructionIndex {
    Phi(usize),
    Inner(usize),
    Terminator,
}

impl fmt::Display for InstructionIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

// TODO: named fields
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct InstructionLocation {
    pub node: NodeId,
    pub index: InstructionIndex,
}

impl fmt::Display for InstructionLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
