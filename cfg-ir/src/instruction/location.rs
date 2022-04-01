use graph::NodeId;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Hash)]
pub enum InstructionIdx {
    Terminator,
    Body(usize),
}

impl fmt::Display for InstructionIdx {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct InstructionLocation(pub NodeId, pub InstructionIdx);

impl fmt::Display for InstructionLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}
