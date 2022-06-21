use std::fmt;

use super::{super::value::ValueId, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct Concat {
    pub dest: ValueId,
    pub values: Vec<ValueId>,
}

impl ValueInfo for Concat {
    fn values_read(&self) -> Vec<ValueId> {
        self.values.clone()
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        self.values.iter_mut().collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for Concat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} <- concat({})",
            self.dest,
            self.values
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
