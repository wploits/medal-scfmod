use std::fmt;

use crate::{Literal, LocalRw, RcLocal, Reduce, RValue};

use super::{Unary, UnaryOperation};

#[derive(Debug, PartialEq, Copy, Clone)]
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
			Self::Add => "+",
			Self::Sub => "-",
			Self::Mul => "*",
			Self::Div => "/",
			Self::Mod => "%",
			Self::Pow => "^",
			Self::Concat => "..",
			Self::Equal => "==",
			Self::NotEqual => "~=",
			Self::LessThanOrEqual => "<=",
			Self::GreaterThanOrEqual => ">=",
			Self::LessThan => "<",
			Self::GreaterThan => ">",
			Self::And => "and",
			Self::Or => "or",
		})
	}
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
	pub left: Box<RValue<'a>>,
	pub right: Box<RValue<'a>>,
	pub operation: BinaryOperation,
}

impl<'a: 'b, 'b> Reduce<'b> for Binary<'a> {
	fn reduce(self) -> RValue<'a> {
		match (self.left, self.right, self.operation) {
			(
				box RValue::Unary(Unary {
									  operation: UnaryOperation::Not,
									  value: left,
								  }),
				box RValue::Unary(Unary {
									  operation: UnaryOperation::Not,
									  value: right,
								  }),
				BinaryOperation::And | BinaryOperation::Or,
			) => Unary {
				value: Box::new(
					Binary {
						left,
						right,
						operation: if self.operation == BinaryOperation::And {
							BinaryOperation::Or
						} else {
							BinaryOperation::And
						},
					}.into()
				),
				operation: UnaryOperation::Not,
			}.reduce(),
			(
				box RValue::Literal(Literal::Boolean(left)),
				box RValue::Literal(Literal::Boolean(right)),
				BinaryOperation::And | BinaryOperation::Or,
			) => if self.operation == BinaryOperation::And {
				RValue::Literal(Literal::Boolean(left && right))
			} else {
				RValue::Literal(Literal::Boolean(left || right))
			}
			(
				box RValue::Literal(Literal::Boolean(left)),
				box right,
				BinaryOperation::And | BinaryOperation::Or,
			) => if self.operation == BinaryOperation::And {
				if !left {
					RValue::Literal(Literal::Boolean(false))
				} else {
					right.reduce()
				}
			} else {
				if left {
					RValue::Literal(Literal::Boolean(true))
				} else {
					right.reduce()
				}
			}
			(
				box left,
				box RValue::Literal(Literal::Boolean(right)),
				BinaryOperation::And | BinaryOperation::Or,
			)
			=> if self.operation == BinaryOperation::And {
				if !right {
					RValue::Literal(Literal::Boolean(false))
				} else {
					left.reduce()
				}
			} else {
				if right {
					RValue::Literal(Literal::Boolean(true))
				} else {
					left.reduce()
				}
			}
			(
				box RValue::Literal(Literal::String(left)),
				box RValue::Literal(Literal::String(right)),
				operation
			)
			if operation == BinaryOperation::Concat => RValue::Literal(Literal::String(left + right)),
			(left, right, operation) => Self {
				left: Box::new(left.reduce()),
				right: Box::new(right.reduce()),
				operation,
			}.into()
		}
	}
}

impl Binary<'_> {
	pub fn precedence(&self) -> usize {
		match self.operation {
			BinaryOperation::Pow => 8,
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
