use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct Concat {
    pub dest: ValueId,
    pub values: Vec<ValueId>,
}

impl ValueInfo for Concat {
    fn values_read(&self) -> Box<[ValueId]> {
        self.values.clone().into_boxed_slice()
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([self.dest])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        self.values.iter_mut().collect()
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.dest])
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
