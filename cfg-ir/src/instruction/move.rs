use std::fmt;

use super::{super::value::ValueId, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct Move {
    pub dest: ValueId,
    pub source: ValueId,
}

impl ValueInfo for Move {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.source]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.source]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {}", self.dest, self.source)
    }
}
