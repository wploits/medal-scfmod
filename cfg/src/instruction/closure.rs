use std::{cell::RefCell, fmt, rc::Rc};

use itertools::Itertools;

use crate::{function::Function, value::ValueId};

use super::value_info::ValueInfo;

#[derive(Debug, Clone, Copy)]
pub enum Upvalue {
    Value(ValueId),
    Upvalue(usize),
}

#[derive(Debug, Clone)]
pub struct Closure<'cfg> {
    pub dest: ValueId,
    pub function: Rc<RefCell<Function<'cfg>>>,
    pub upvalues: Vec<Upvalue>,
}

impl ValueInfo for Closure<'_> {
    fn values_read(&self) -> Vec<ValueId> {
        self.upvalues
            .iter()
            .filter_map(|&u| match u {
                Upvalue::Value(v) => Some(v),
                Upvalue::Upvalue(_) => None,
            })
            .collect::<Vec<_>>()
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        self.upvalues
            .iter_mut()
            .filter_map(|u| match u {
                Upvalue::Value(v) => Some(v),
                Upvalue::Upvalue(_) => None,
            })
            .collect::<Vec<_>>()
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Closure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} <- Function (capturing {})",
            self.dest,
            self.upvalues.iter().map(|v| format!("{:?}", v)).join(", ")
        )
    }
}
