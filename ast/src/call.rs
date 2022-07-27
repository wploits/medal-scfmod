use itertools::Itertools;
use std::fmt;

use crate::{LocalRw, RcLocal};

use super::RValue;

#[derive(Debug, Clone, PartialEq)]
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

impl<'a> LocalRw<'a> for Call<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.value
            .values_read()
            .into_iter()
            .chain(self.arguments.iter().flat_map(|r| r.values_read()))
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.value
            .values_read_mut()
            .into_iter()
            .chain(self.arguments.iter_mut().flat_map(|r| r.values_read_mut()))
            .collect()
    }
}

impl fmt::Display for Call<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.value, self.arguments.iter().join(", "))
    }
}
