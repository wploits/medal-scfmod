use std::{borrow::Cow, fmt};

// A struct that represents a constant value.
#[derive(Debug, Clone)]
pub enum Constant<'cfg> {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Cow<'cfg, str>),
}

impl From<bool> for Constant<'_> {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<f64> for Constant<'_> {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl fmt::Display for Constant<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Nil => write!(f, "nil"),
            Constant::Boolean(value) => write!(f, "{}", value),
            Constant::Number(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "{}", value),
        }
    }
}
