use itertools::Itertools;

use crate::{LocalRw, RValue, RcLocal, SideEffects, Traverse};

#[derive(Debug, Clone, PartialEq)]
pub struct SetList {
    pub table: RcLocal,
    pub index: usize,
    pub values: Vec<RValue>,
}

impl SetList {
    pub fn new(table: RcLocal, index: usize, values: Vec<RValue>) -> Self {
        Self {
            table,
            index,
            values,
        }
    }
}

impl LocalRw for SetList {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.values
            .iter()
            .flat_map(|rvalue| rvalue.values_read())
            .chain(std::iter::once(&self.table))
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.values
            .iter_mut()
            .flat_map(|rvalue| rvalue.values_read_mut())
            .chain(std::iter::once(&mut self.table))
            .collect()
    }
}

impl SideEffects for SetList {
    fn has_side_effects(&self) -> bool {
        self.values.iter().any(|rvalue| rvalue.has_side_effects())
    }
}

impl Traverse for SetList {
    fn rvalues(&self) -> Vec<&RValue> {
        self.values.iter().collect()
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        self.values.iter_mut().collect()
    }
}

impl std::fmt::Display for SetList {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "__set_list({}, {}, {{{}}})",
            self.table,
            self.index,
            self.values.iter().join(", ")
        )
    }
}
