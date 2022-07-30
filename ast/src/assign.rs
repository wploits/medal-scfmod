use std::fmt;

use itertools::Itertools;

use crate::{RcLocal, SideEffects, Traverse};

use super::{LValue, LocalRw, RValue};

#[derive(Debug, Clone)]
pub struct Assign {
    pub left: Vec<LValue>,
    pub right: Vec<RValue>,
}

impl Assign {
    pub fn new(left: Vec<LValue>, right: Vec<RValue>) -> Self {
        Self { left, right }
    }
}

impl Traverse for Assign {
    fn lvalues<'a>(&'a mut self) -> Vec<&'a mut LValue> {
        self.left.iter_mut().collect()
    }

    fn rvalues<'a>(&'a mut self) -> Vec<&'a mut RValue> {
        self.right.iter_mut().collect()
    }
}

impl SideEffects for Assign {
    fn has_side_effects(&self) -> bool {
        self.right.iter().any(|r| r.has_side_effects()) || self.left.iter().any(|l| l.has_side_effects())
    }
}

impl LocalRw for Assign {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.left
            .iter()
            .flat_map(|l| l.values_read())
            .chain(self.right.iter().flat_map(|r| r.values_read()))
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.left
            .iter_mut()
            .flat_map(|l| l.values_read_mut())
            .chain(self.right.iter_mut().flat_map(|r| r.values_read_mut()))
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        self.left.iter().flat_map(|l| l.values_written()).collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        self.left
            .iter_mut()
            .flat_map(|l| l.values_written_mut())
            .collect()
    }
}

impl fmt::Display for Assign {
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
