use std::fmt;

use crate::{LocalRw, RValue, RcLocal};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOperation {
    And,
    Or,
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperation::And => "and",
                BinaryOperation::Or => "or",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub left: Box<RValue<'a>>,
    pub right: Box<RValue<'a>>,
    pub operation: BinaryOperation,
}

impl<'a> Binary<'a> {
    pub fn new(left: RValue<'a>, right: RValue<'a>, operation: BinaryOperation) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operation,
        }
    }
}

impl<'a> LocalRw<'a> for Binary<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.left
            .values_read()
            .into_iter()
            .chain(self.right.values_read())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.left
            .values_read_mut()
            .into_iter()
            .chain(self.right.values_read_mut())
            .collect()
    }
}

impl fmt::Display for Binary<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operation, self.right)
    }
}
