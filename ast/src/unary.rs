use std::fmt;

use crate::{Literal, LocalRw, RValue, RcLocal, Reduce, SideEffects, Traverse};

use super::{Binary, BinaryOperation};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
pub struct Unary {
    pub value: Box<RValue>,
    pub operation: UnaryOperation,
}

impl SideEffects for Unary {
    fn has_side_effects(&self) -> bool {
        self.value.has_side_effects()
    }
}

impl Traverse for Unary {
    fn rvalues(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.value]
    }
}

impl Reduce for Unary {
    fn reduce(self) -> RValue {
        let is_not_expression = |expression: &RValue| {
            matches!(
                expression,
                RValue::Unary(Unary {
                    value: _,
                    operation: UnaryOperation::Not,
                })
            )
        };

        match (*self.value, self.operation) {
            (RValue::Literal(Literal::Boolean(value)), UnaryOperation::Not) => {
                RValue::Literal(Literal::Boolean(!value))
            }
            (
                RValue::Unary(Unary {
                    box value,
                    operation: UnaryOperation::Not,
                }),
                UnaryOperation::Not,
            ) => value.reduce(),
            (RValue::Literal(Literal::Number(value)), UnaryOperation::Negate) => {
                RValue::Literal(Literal::Number(-value))
            }
            (RValue::Literal(Literal::String(value)), UnaryOperation::Length) => {
                RValue::Literal(Literal::Number(value.len() as f64))
            }
            (
                RValue::Binary(Binary {
                    left,
                    right,
                    operation,
                }),
                UnaryOperation::Not,
            ) if (operation == BinaryOperation::And || operation == BinaryOperation::Or)
                && (is_not_expression(left.as_ref()) || is_not_expression(right.as_ref())) =>
            {
                Binary {
                    left: Box::new(
                        Unary {
                            value: left,
                            operation: UnaryOperation::Not,
                        }
                        .reduce(),
                    ),
                    right: Box::new(
                        Unary {
                            value: right,
                            operation: UnaryOperation::Not,
                        }
                        .reduce(),
                    ),
                    operation: if operation == BinaryOperation::And {
                        BinaryOperation::Or
                    } else {
                        BinaryOperation::And
                    },
                }
                .reduce()
            }
            (value, operation) => Self {
                value: Box::new(value.reduce()),
                operation,
            }
            .into(),
        }
    }
}

impl Unary {
    pub fn new(value: RValue, operation: UnaryOperation) -> Self {
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

impl LocalRw for Unary {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.value.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.value.values_read_mut()
    }
}

impl fmt::Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            self.operation,
            if self.precedence() > self.value.precedence() && self.value.precedence() != 0 {
                format!("({})", self.value)
            } else {
                format!("{}", self.value)
            }
        )
    }
}
