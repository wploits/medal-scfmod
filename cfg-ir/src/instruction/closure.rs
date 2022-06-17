use std::{fmt, rc::Rc};

use crate::{function::Function, value::ValueId};

use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct Closure<'cfg> {
    pub dest: ValueId,
    pub function: Rc<Function<'cfg>>,
}

impl ValueInfo for Closure<'_> {
    fn values_read(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Closure<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- Function", self.dest)
    }
}
