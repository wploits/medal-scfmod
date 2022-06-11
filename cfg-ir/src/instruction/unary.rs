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
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([self.value])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([self.dest])
    }

    // TODO: generate using macro?
    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.value])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.dest])
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {} {}", self.dest, self.op, self.value)
    }
}
