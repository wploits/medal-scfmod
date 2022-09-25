use std::fmt;

use crate::{Literal, LocalRw, RValue, RcLocal, Reduce, SideEffects, Traverse};

use super::{Unary, UnaryOperation};

#[derive(Debug, PartialEq, Eq, PartialOrd, Copy, Clone)]
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
        write!(
            f,
            "{}",
            match self {
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
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<RValue>,
    pub right: Box<RValue>,
    pub operation: BinaryOperation,
}

impl Traverse for Binary {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.left, &mut self.right]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.left, &self.right]
    }
}

impl SideEffects for Binary {
    fn has_side_effects(&self) -> bool {
        self.left.has_side_effects() || self.right.has_side_effects()
    }
}

impl<'a: 'b, 'b> Reduce for Binary {
    fn reduce(self) -> RValue {
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
                    }
                    .into(),
                ),
                operation: UnaryOperation::Not,
            }
            .into(),
            (
                box RValue::Literal(Literal::Boolean(left)),
                box RValue::Literal(Literal::Boolean(right)),
                BinaryOperation::And | BinaryOperation::Or,
            ) => Literal::Boolean(if self.operation == BinaryOperation::And {
                left && right
            } else {
                left || right
            })
            .into(),
            (
                box RValue::Literal(Literal::Boolean(left)),
                box right,
                BinaryOperation::And | BinaryOperation::Or,
            ) => {
                if self.operation == BinaryOperation::And {
                    if !left {
                        RValue::Literal(Literal::Boolean(false))
                    } else {
                        right.reduce()
                    }
                } else if left {
                    RValue::Literal(Literal::Boolean(true))
                } else {
                    right.reduce()
                }
            }
            (
                box left,
                box RValue::Literal(Literal::Boolean(right)),
                BinaryOperation::And | BinaryOperation::Or,
            ) => {
                if self.operation == BinaryOperation::And {
                    if !right {
                        RValue::Literal(Literal::Boolean(false))
                    } else {
                        left.reduce()
                    }
                } else if right {
                    RValue::Literal(Literal::Boolean(true))
                } else {
                    left.reduce()
                }
            }
            (
                box RValue::Literal(Literal::String(left)),
                box RValue::Literal(Literal::String(right)),
                BinaryOperation::Concat,
            ) => RValue::Literal(Literal::String(format!("{}{}", left, right))),
            (left, right, operation) => Self {
                left: Box::new(left.reduce()),
                right: Box::new(right.reduce()),
                operation,
            }
            .into(),
        }
    }
}

impl Binary {
    pub fn new(left: RValue, right: RValue, operation: BinaryOperation) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
            operation,
        }
    }

    pub fn precedence(&self) -> usize {
        match self.operation {
            BinaryOperation::Pow => 8,
            BinaryOperation::Mul | BinaryOperation::Div | BinaryOperation::Mod => 6,
            BinaryOperation::Add | BinaryOperation::Sub => 5,
            BinaryOperation::Concat => 4,
            BinaryOperation::LessThan
            | BinaryOperation::GreaterThan
            | BinaryOperation::LessThanOrEqual
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::Equal
            | BinaryOperation::NotEqual => 3,
            BinaryOperation::And => 2,
            BinaryOperation::Or => 1,
        }
    }
}

impl LocalRw for Binary {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.left
            .values_read()
            .into_iter()
            .chain(self.right.values_read().into_iter())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.left
            .values_read_mut()
            .into_iter()
            .chain(self.right.values_read_mut().into_iter())
            .collect()
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: rename `expression` to `rvalue`
        let parentheses = |expression: &RValue| {
            if self.precedence() > expression.precedence() && expression.precedence() != 0 {
                format!("({})", expression)
            } else {
                format!("{}", expression)
            }
        };

        write!(
            f,
            "{} {} {}",
            parentheses(self.left.as_ref()),
            self.operation,
            parentheses(self.right.as_ref()),
        )
    }
}
