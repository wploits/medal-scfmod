use super::{LValue, RValue};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Index<'a> {
    pub left: Box<LValue<'a>>,
    pub right: Box<RValue<'a>>,
}

impl<'a> Index<'a> {
    pub fn new(left: LValue<'a>, right: RValue<'a>) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl fmt::Display for Index<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.left, self.right)
    }
}
