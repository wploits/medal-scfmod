use graph::NodeId;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum InstructionIndex {
    Phi(usize),
    Inner(usize),
    Terminator,
}

impl PartialOrd for InstructionIndex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match self {
            InstructionIndex::Phi(_i) => todo!(),
            InstructionIndex::Inner(i) => match other {
                InstructionIndex::Phi(_) => std::cmp::Ordering::Greater,
                InstructionIndex::Inner(j) => i.cmp(&j),
                InstructionIndex::Terminator => std::cmp::Ordering::Less,
            },
            InstructionIndex::Terminator => match other {
                InstructionIndex::Phi(_) => todo!(),
                InstructionIndex::Inner(_) => std::cmp::Ordering::Greater,
                InstructionIndex::Terminator => std::cmp::Ordering::Equal,
            },
        })
    }
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
