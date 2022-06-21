use itertools::Itertools;
use std::fmt;

use super::RValue;

#[derive(Debug, Clone)]
pub struct Call<'a> {
    pub value: Box<RValue<'a>>,
    pub arguments: Vec<RValue<'a>>,
}

impl<'a> Call<'a> {
    pub fn new(value: RValue<'a>, arguments: Vec<RValue<'a>>) -> Self {
        Self {
            value: Box::new(value),
            arguments,
        }
    }
}

impl fmt::Display for Call<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.value, self.arguments.iter().join(", "))
    }
}
