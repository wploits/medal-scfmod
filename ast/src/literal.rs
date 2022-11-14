use derive_more::From;
use enum_as_inner::EnumAsInner;
use std::fmt;

use crate::{
    formatter::Formatter, type_system::Infer, LocalRw, Reduce, SideEffects, Traverse, Type,
    TypeSystem,
};

#[derive(Debug, From, Clone, PartialEq, PartialOrd, EnumAsInner)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(Vec<u8>),
}

impl Reduce for Literal {
    fn reduce(self) -> crate::RValue {
        self.into()
    }

    fn reduce_condition(self) -> crate::RValue {
        Literal::Boolean(match self {
            Literal::Boolean(false) | Literal::Nil => false,
            Literal::Boolean(true) | Literal::Number(_) | Literal::String(_) => true,
        })
        .into()
    }
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
            Literal::String(value) => {
                write!(
                    f,
                    "\"{}\"",
                    Formatter::<fmt::Formatter>::escape_string(value)
                )
            }
        }
    }
}
