use derive_more::From;
use enum_as_inner::EnumAsInner;
use std::{borrow::Cow, fmt};

use crate::{LocalRw, has_side_effects, SideEffects};

#[derive(Debug, From, Clone, PartialEq, EnumAsInner)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl LocalRw for Literal {}

impl SideEffects for Literal {}

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
