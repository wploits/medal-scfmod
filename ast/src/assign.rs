use std::fmt;

use itertools::Itertools;

use crate::RcLocal;

use super::{LValue, LocalRw, RValue};

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub left: Vec<LValue<'a>>,
    pub right: Vec<RValue<'a>>,
}

impl<'a> Assign<'a> {
    pub fn new(left: Vec<LValue<'a>>, right: Vec<RValue<'a>>) -> Self {
        Self { left, right }
    }
}

impl<'a> LocalRw<'a> for Assign<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.left
            .iter()
            .flat_map(|l| l.values_read())
            .chain(self.right.iter().flat_map(|r| r.values_read()))
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.left
            .iter_mut()
            .flat_map(|l| l.values_read_mut())
            .chain(self.right.iter_mut().flat_map(|r| r.values_read_mut()))
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal<'a>> {
        self.left.iter().flat_map(|l| l.values_written()).collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.left
            .iter_mut()
            .flat_map(|l| l.values_written_mut())
            .collect()
    }
}

impl fmt::Display for Assign<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            if self.left.is_empty() {
                String::new()
            } else {
                format!(
                    "{} = ",
                    self.left.iter().map(ToString::to_string).join(", ")
                )
            },
            self.right.iter().map(ToString::to_string).join(", ")
        )
    }
}
