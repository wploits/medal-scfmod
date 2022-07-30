use crate::{LocalRw, RValue, RcLocal, has_side_effects};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Table(pub Vec<(Option<String>, RValue)>);

impl LocalRw for Table {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.0.iter().flat_map(|(_, r)| r.values_read()).collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.0
            .iter_mut()
            .flat_map(|(_, r)| r.values_read_mut())
            .collect()
    }
}

// TODO
has_side_effects!(Table);

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.0
                .iter()
                .map(|(key, value)| match key {
                    Some(key) => format!("{} = {}", key, value),
                    None => value.to_string(),
                })
                .join(", ")
        )
    }
}
