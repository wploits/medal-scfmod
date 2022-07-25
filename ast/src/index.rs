use crate::{LocalRw, RcLocal};

use super::RValue;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Index<'a> {
    pub left: Box<RValue<'a>>,
    pub right: Box<RValue<'a>>,
}

impl<'a> Index<'a> {
    pub fn new(left: RValue<'a>, right: RValue<'a>) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl<'a> LocalRw<'a> for Index<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.left
            .values_read()
            .into_iter()
            .chain(self.right.values_read().into_iter())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.left
            .values_read_mut()
            .into_iter()
            .chain(self.right.values_read_mut().into_iter())
            .collect()
    }
}

impl fmt::Display for Index<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.left, self.right)
    }
}
