use std::fmt;

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
        self.arguments.clone()
    }

    fn values_written(&self) -> Vec<ValueId> {
        self.return_values.clone()
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        self.arguments.iter_mut().collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        self.return_values.iter_mut().collect()
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} <- {{}}",
            self.return_values
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}
