use itertools::Itertools;
use std::fmt;

use crate::{LocalRw, RcLocal};

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

impl<'a> LocalRw<'a> for Return<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.values.iter().flat_map(|r| r.values_read()).collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.values
            .iter_mut()
            .flat_map(|r| r.values_read_mut())
            .collect()
    }
}

impl fmt::Display for Return<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {}", self.values.iter().join(", "))
    }
}
