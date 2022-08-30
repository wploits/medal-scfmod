use std::fmt;

use itertools::Itertools;

use crate::{type_system::Type, RcLocal, SideEffects, Traverse};

use super::{LValue, LocalRw, RValue};

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub left: Vec<(LValue, Option<Type>)>,
    pub right: Vec<RValue>,
}

impl Assign {
    pub fn new(left: Vec<LValue>, right: Vec<RValue>) -> Self {
        Self {
            left: left.into_iter().map(|v| (v, None)).collect(),
            right,
        }
    }
}

impl Traverse for Assign {
    fn lvalues_mut(&mut self) -> Vec<&mut LValue> {
        self.left.iter_mut().map(|x| &mut x.0).collect()
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        self.right.iter_mut().collect()
    }

    fn rvalues(&self) -> Vec<&RValue> {
        self.right.iter().collect()
    }
}

impl SideEffects for Assign {
    fn has_side_effects(&self) -> bool {
        self.right.iter().any(|r| r.has_side_effects())
            || self.left.iter().any(|(l, _)| l.has_side_effects())
    }
}

impl LocalRw for Assign {
    fn values_read<'a>(&'a self) -> Vec<&'a RcLocal> {
        self.left
            .iter()
            .flat_map(|(l, _)| l.values_read())
            .chain(self.right.iter().flat_map(|r| r.values_read()))
            .collect()
    }

    fn values_read_mut<'a>(&'a mut self) -> Vec<&'a mut RcLocal> {
        self.left
            .iter_mut()
            .flat_map(|(l, _)| l.values_read_mut())
            .chain(self.right.iter_mut().flat_map(|r| r.values_read_mut()))
            .collect()
    }

    fn values_written<'a>(&'a self) -> Vec<&'a RcLocal> {
        self.left
            .iter()
            .flat_map(|(l, _)| l.values_written())
            .collect()
    }

    fn values_written_mut<'a>(&'a mut self) -> Vec<&'a mut RcLocal> {
        self.left
            .iter_mut()
            .flat_map(|(l, _)| l.values_written_mut())
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
                    self.left
                        .iter()
                        .map(|(l, annotation)| {
                            format!(
                                "{}{}",
                                l,
                                annotation
                                    .as_ref()
                                    .map(|t| format!(": {}", t))
                                    .unwrap_or_default(),
                            )
                        })
                        .join(", ")
                )
            },
            self.right.iter().map(ToString::to_string).join(", ")
        )
    }
}
