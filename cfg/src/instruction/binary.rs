use std::fmt;

use super::{super::value::ValueId, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Equal,
    LesserOrEqual,
    LesserThan,
    LogicalAnd,
    LogicalOr,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Pow => write!(f, "^"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::LesserOrEqual => write!(f, "<="),
            BinaryOp::LesserThan => write!(f, "<"),
            BinaryOp::LogicalAnd => write!(f, "and"),
            BinaryOp::LogicalOr => write!(f, "or"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub dest: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
    pub op: BinaryOp,
}

impl ValueInfo for Binary {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.lhs, self.rhs]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.lhs, &mut self.rhs]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {} {} {}", self.dest, self.lhs, self.op, self.rhs)
    }
}
