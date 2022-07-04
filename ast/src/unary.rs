use crate::{LocalRw, RValue, RcLocal};
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

impl<'a> LocalRw<'a> for Unary<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.value.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.value.values_read_mut()
    }
}

impl fmt::Display for Unary<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.operation, self.value)
    }
}
