use derive_more::From;
use enum_as_inner::EnumAsInner;
use std::fmt;

use crate::{type_system::Infer, LocalRw, SideEffects, Traverse, Type, TypeSystem};

#[derive(Debug, From, Clone, PartialEq, PartialOrd, EnumAsInner)]
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

fn escape_string(string: &str) -> String {
    let mut s = String::with_capacity(string.len());
    for c in string.chars() {
        if c == ' ' || (c.is_ascii_graphic() && c != '\\' && c != '\'' && c != '\"') {
            s.push(c);
        } else {
            match c {
                '\n' => s.push_str("\\n"),
                '\r' => s.push_str("\\r"),
                '\t' => s.push_str("\\t"),
                '\"' => s.push_str("\\\""),
                '\'' => s.push_str("\\'"),
                _ => s.push_str(&format!("\\x{:02x}", c as u8)),
            };
        }
    }
    s
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(value) => write!(f, "{}", value),
            Literal::Number(value) => write!(f, "{}", value),
            Literal::String(value) => {
                write!(f, "\"{}\"", escape_string(value))
            }
        }
    }
}
