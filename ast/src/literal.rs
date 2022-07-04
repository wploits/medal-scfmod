use derive_more::From;
use std::{borrow::Cow, fmt};

use crate::LocalRw;

#[derive(Debug, From, Clone)]
pub enum Literal<'a> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Cow<'a, str>),
}

impl<'a> From<&'a str> for Literal<'a> {
    fn from(value: &'a str) -> Self {
        Self::String(value.into())
    }
}

impl LocalRw<'_> for Literal<'_> {}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(value) => write!(f, "{}", value),
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "\"{}\"", value),
        }
    }
}
