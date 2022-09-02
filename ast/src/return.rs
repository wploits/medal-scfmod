use itertools::Itertools;
use std::fmt;

use crate::{has_side_effects, LocalRw, RcLocal, Traverse};

use super::RValue;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Return {
    pub values: Vec<RValue>,
}

has_side_effects!(Return);

impl Return {
    pub fn new(values: Vec<RValue>) -> Self {
        Self { values }
    }
}

impl Traverse for Return {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        self.values.iter_mut().rev().collect()
    }

    fn rvalues(&self) -> Vec<&RValue> {
        self.values.iter().rev().collect()
    }
}

impl LocalRw for Return {
    fn values_read<'a>(&'a self) -> Vec<&'a RcLocal> {
        self.values.iter().rev().flat_map(|r| r.values_read()).collect()
    }

    fn values_read_mut<'a>(&'a mut self) -> Vec<&'a mut RcLocal> {
            self.values
                .iter_mut()
                .rev()
                .flat_map(|r| r.values_read_mut()).collect()
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {}", self.values.iter().join(", "))
    }
}
