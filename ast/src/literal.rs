use std::collections::HashMap;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use std::fmt;

use crate::{LocalRw, RcLocal, SideEffects, Traverse, Type, type_system::Infer, TypeSystem};

#[derive(Debug, From, Clone, PartialEq, EnumAsInner)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Infer for Literal {
    fn infer<'a: 'b, 'b>(&'a mut self, _: &mut TypeSystem<'b>) -> Type {
        match self {
            Literal::Nil => Type::Nil,
            Literal::Boolean(_) => Type::Boolean,
            Literal::Number(_) => Type::Number,
            Literal::String(_) => Type::String,
        }
    }
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl LocalRw for Literal {}

impl SideEffects for Literal {}

impl Traverse for Literal {}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(value) => write!(f, "{}", value),
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "\"{}\"", value),
        }
    }
}
