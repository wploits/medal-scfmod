use super::{LValue, RValue};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub left: LValue<'a>,
    pub right: RValue<'a>,
}

impl<'a> Assign<'a> {
    pub fn new(left: LValue<'a>, right: RValue<'a>) -> Self {
        Self { left, right }
    }
}

impl<'a> fmt::Display for Assign<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.left, self.right)
    }
}
