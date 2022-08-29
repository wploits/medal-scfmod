use itertools::Itertools;
use std::fmt;

use crate::{LocalRw, RcLocal, SideEffects, Traverse};

use super::RValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub value: Box<RValue>,
    pub arguments: Vec<RValue>,
}

impl Call {
    pub fn new(value: RValue, arguments: Vec<RValue>) -> Self {
        Self {
            value: Box::new(value),
            arguments,
        }
    }
}

impl SideEffects for Call {
    fn has_side_effects(&self) -> bool {
        self.value.has_side_effects() || self.arguments.iter().any(|arg| arg.has_side_effects())
    }
}

impl Traverse for Call {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        std::iter::once(self.value.as_mut())
            .chain(self.arguments.iter_mut())
            .collect()
    }
}

impl LocalRw for Call {
    fn values_read<'a>(&'a self) -> Box<dyn Iterator<Item = &'a RcLocal> + 'a> {
        Box::new(
            self.value
                .values_read()
                .chain(self.arguments.iter().flat_map(|r| r.values_read())),
        )
    }

    fn values_read_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut RcLocal> + 'a> {
        Box::new(
            self.value
                .values_read_mut()
                .chain(self.arguments.iter_mut().flat_map(|r| r.values_read_mut())),
        )
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.value, self.arguments.iter().join(", "))
    }
}
