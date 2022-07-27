use std::fmt;

use crate::{Literal, LocalRw, RcLocal, Reduce, RValue};

use super::{Binary, BinaryOperation};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOperation {
    Not,
    Negate,
    Length,
}

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Not => write!(f, "not "),
            Self::Negate => write!(f, "-"),
            Self::Length => write!(f, "#"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub value: Box<RValue<'a>>,
    pub operation: UnaryOperation,
}

impl<'a: 'b, 'b> Reduce<'b> for Unary<'a> {
	fn reduce(self) -> RValue<'a> {
		match (*self.value, self.operation) {
			(RValue::Literal(Literal::Boolean(value)), UnaryOperation::Not) => RValue::Literal(Literal::Boolean(!value)),
			(
				RValue::Unary(
					Unary {
						box value,
						operation: UnaryOperation::Not,
					}),
				UnaryOperation::Not) => value.reduce(),
			(RValue::Literal(Literal::Number(value)), UnaryOperation::Negate) => RValue::Literal(Literal::Number(-value)),
			(RValue::Literal(Literal::String(value)), UnaryOperation::Length) => RValue::Literal(Literal::Number(value.len() as f64)),
			(
				RValue::Binary(Binary {
								   left,
								   right,
								   operation,
							   }),
				UnaryOperation::Not,
			) if operation == BinaryOperation::And || operation == BinaryOperation::Or => Binary {
				left: Box::new(Unary {
					value: left,
					operation: UnaryOperation::Not,
				}.reduce()),
				right: Box::new(Unary {
					value: right,
					operation: UnaryOperation::Not,
				}.reduce()),
				operation: if operation == BinaryOperation::And { BinaryOperation::Or } else { BinaryOperation::And },
			}.reduce(),
			(value, operation) => Self {
				value: Box::new(value.reduce()),
				operation,
			}.into(),
		}
	}
}

impl<'a> Unary<'a> {
	pub fn new(value: RValue<'a>, operation: UnaryOperation) -> Self {
		Self {
			value: Box::new(value),
			operation,
		}
	}

	pub fn precedence(&self) -> usize {
		match self.operation {
			UnaryOperation::Not | UnaryOperation::Negate => 7,
			_ => 0,
		}
	}
}

impl<'a> LocalRw<'a> for Unary<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.value.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.value.values_read_mut()
    }
}

impl fmt::Display for Unary<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}{}", self.operation, if self.precedence() > self.value.precedence() && self.value.precedence() != 0 {
			format!("({})", self.value)
		} else {
			format!("{}", self.value)
		})
	}
}
