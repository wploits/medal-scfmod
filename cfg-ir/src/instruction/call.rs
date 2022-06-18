use std::fmt;

use itertools::Itertools;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct Call {
    pub function: ValueId,
    pub arguments: Vec<ValueId>,
    pub variadic_arguments: bool,
    pub return_values: Vec<ValueId>,
    pub variadic_return: bool,
}

impl ValueInfo for Call {
    fn values_read(&self) -> Vec<ValueId> {
        let mut read = self.arguments.clone();
        read.push(self.function);
        read
    }

    fn values_written(&self) -> Vec<ValueId> {
        self.return_values.clone()
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        let mut read = self.arguments.iter_mut().collect::<Vec<_>>();
        read.push(&mut self.function);
        read
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        self.return_values.iter_mut().collect()
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.return_values.is_empty() {
            write!(
                f,
                "{} <- ",
                self.return_values
                    .iter()
                    .map(|v| v.to_string())
                    .join(", "),
            )?;
        }
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|v| v.to_string())
                .join(", ")
        )
    }
}
