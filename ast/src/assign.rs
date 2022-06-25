use std::fmt;

use itertools::Itertools;

use super::{LValue, RValue};

#[derive(Debug, Clone)]
pub struct Assign<'a> {
	pub left: Vec<LValue<'a>>,
	pub right: Vec<RValue<'a>>,
}

impl<'a> Assign<'a> {
	pub fn new(left: Vec<LValue<'a>>, right: Vec<RValue<'a>>) -> Self {
		Self { left, right }
	}
}

impl<'a> fmt::Display for Assign<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f,
			   "{} = {}",
			   self.left.iter().map(ToString::to_string).join(", "),
			   self.right.iter().map(ToString::to_string).join(", ")
        )
	}
}
