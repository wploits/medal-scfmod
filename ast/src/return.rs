use itertools::Itertools;
use std::fmt;

use super::RValue;

#[derive(Debug, Clone, Default)]
pub struct Return<'a> {
    pub values: Vec<RValue<'a>>,
}

impl<'a> Return<'a> {
    pub fn new(values: Vec<RValue<'a>>) -> Self {
        Self { values }
    }
}

impl fmt::Display for Return<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {}", self.values.iter().join(", "))
    }
}
