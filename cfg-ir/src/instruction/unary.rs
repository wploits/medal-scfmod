use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Minus,
    LogicalNot,
    Len,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::LogicalNot => write!(f, "not"),
            UnaryOp::Len => write!(f, "#"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub dest: ValueId,
    pub value: ValueId,
    pub op: UnaryOp,
}

impl ValueInfo for Unary {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.value]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    // TODO: generate using macro?
    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.value]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {} {}", self.dest, self.op, self.value)
    }
}
