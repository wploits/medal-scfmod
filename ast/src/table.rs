use std::fmt;
use itertools::Itertools;
use crate::RValue;

#[derive(Debug, Clone)]
pub struct Table<'a>(pub Vec<(Option<&'a str>, RValue<'a>)>);

impl fmt::Display for Table<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{{{}}}",
			self.0.iter()
				.map(|(key, value)| match key {
					Some(key) => format!("{} = {}", key, value),
					None => value.to_string(),
				}).join(", ")
		)
	}
}
