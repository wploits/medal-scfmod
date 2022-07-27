use crate::{LocalRw, RValue, RcLocal};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Table<'a>(pub Vec<(Option<&'a str>, RValue<'a>)>);

impl<'a> LocalRw<'a> for Table<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.0.iter().flat_map(|(_, r)| r.values_read()).collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.0
            .iter_mut()
            .flat_map(|(_, r)| r.values_read_mut())
            .collect()
    }
}

impl fmt::Display for Table<'_> {
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
