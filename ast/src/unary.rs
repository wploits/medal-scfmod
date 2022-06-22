use crate::RValue;
use std::fmt;

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Not,
    Negate,
    Length,
}

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Not => write!(f, "not "),
            Self::Negate => write!(f, "-"),
            Self::Length => write!(f, "#"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary<'a> {
    pub value: Box<RValue<'a>>,
    pub operation: UnaryOperation,
}

impl<'a> Unary<'a> {
    pub fn new(value: RValue<'a>, operation: UnaryOperation) -> Self {
        Self {
            value: Box::new(value),
            operation,
        }
    }
}

impl fmt::Display for Unary<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.operation, self.value)
    }
}
