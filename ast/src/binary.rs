use std::fmt;

use crate::{LocalRw, RcLocal, RValue};

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Pow,
	Concat,
	Equal,
	NotEqual,
	LessThanOrEqual,
	GreaterThanOrEqual,
	LessThan,
	GreaterThan,
	And,
	Or,
}

impl fmt::Display for BinaryOperation {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", match self {
			BinaryOperation::Add => "+",
			BinaryOperation::Sub => "-",
			BinaryOperation::Mul => "*",
			BinaryOperation::Div => "/",
			BinaryOperation::Mod => "%",
			BinaryOperation::Pow => "^",
			BinaryOperation::Concat => "..",
			BinaryOperation::Equal => "==",
			BinaryOperation::NotEqual => "~=",
			BinaryOperation::LessThanOrEqual => "<=",
			BinaryOperation::GreaterThanOrEqual => ">=",
			BinaryOperation::LessThan => "<",
			BinaryOperation::GreaterThan => ">",
			BinaryOperation::And => "and",
			BinaryOperation::Or => "or",
		})
	}
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
	pub left: Box<RValue<'a>>,
	pub right: Box<RValue<'a>>,
	pub operation: BinaryOperation,
}

impl Binary<'_> {
	pub fn precedence(&self) -> usize {
		match self.operation {
			BinaryOperation::Pow => 7,
			BinaryOperation::Mul | BinaryOperation::Div | BinaryOperation::Mod => 6,
			BinaryOperation::Add | BinaryOperation::Sub => 5,
			BinaryOperation::Concat => 4,
			BinaryOperation::LessThan | BinaryOperation::GreaterThan
			| BinaryOperation::LessThanOrEqual | BinaryOperation::GreaterThanOrEqual
			| BinaryOperation::Equal | BinaryOperation::NotEqual => 3,
			BinaryOperation::And => 2,
			BinaryOperation::Or => 1,
		}
	}
}

impl<'a> LocalRw<'a> for Binary<'a> {
	fn values_read(&self) -> Vec<&RcLocal<'a>> {
		self.left.values_read().into_iter().chain(self.right.values_read()).collect()
	}

	fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
		self.left.values_read_mut().into_iter().chain(self.right.values_read_mut()).collect()
	}
}

impl fmt::Display for Binary<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let parentheses = |expression: &RValue| if self.precedence() > expression.precedence() && expression.precedence() != 0 {
			format!("({})", expression)
		} else {
			format!("{}", expression)
		};

		write!(f,
			   "{} {} {}",
			   parentheses(self.left.as_ref()),
			   self.operation,
			   parentheses(self.right.as_ref()),
		)
	}
}
