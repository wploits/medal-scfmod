use std::fmt;

// A struct that represents a constant value.
#[derive(Debug, Clone)]
pub enum Constant {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl From<bool> for Constant {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<f64> for Constant {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Constant {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Nil => write!(f, "nil"),
            Constant::Boolean(value) => write!(f, "{}", value),
            Constant::Number(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "{}", value),
        }
    }
}
