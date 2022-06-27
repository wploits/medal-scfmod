use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use std::{fmt, rc::Rc};

mod assign;
mod r#break;
mod call;
mod r#continue;
mod global;
mod goto;
mod r#if;
mod index;
mod literal;
mod local;
pub mod local_allocator;
mod name_gen;
mod r#return;
mod table;
mod unary;
mod r#while;

pub use assign::*;
pub use call::*;
pub use global::*;
pub use goto::*;
pub use index::*;
pub use literal::*;
pub use local::*;
pub use r#break::*;
pub use r#continue::*;
pub use r#if::*;
pub use r#return::*;
pub use r#while::*;
pub use table::*;
pub use unary::*;

#[derive(Debug, From, Clone)]
pub enum RValue<'a> {
    Local(Rc<Local<'a>>),
    Global(Global<'a>),
    Call(Call<'a>),
    Table(Table<'a>),
    Literal(Literal<'a>),
    Index(Index<'a>),
    Unary(Unary<'a>),
}

impl fmt::Display for RValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RValue::Local(local) => write!(f, "{}", local),
            RValue::Global(global) => write!(f, "{}", global),
            RValue::Literal(literal) => write!(f, "{}", literal),
            RValue::Call(call) => write!(f, "{}", call),
            RValue::Table(table) => write!(f, "{}", table),
            RValue::Index(index) => write!(f, "{}", index),
            RValue::Unary(unary) => write!(f, "{}", unary),
        }
    }
}

#[derive(Debug, From, Clone, EnumAsInner)]
pub enum LValue<'a> {
    Local(Rc<Local<'a>>),
    Global(Global<'a>),
}

impl fmt::Display for LValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LValue::Local(local) => write!(f, "{}", local),
            LValue::Global(global) => write!(f, "{}", global),
        }
    }
}

#[derive(Debug, From, Clone, EnumAsInner)]
pub enum Statement<'a> {
    Call(Call<'a>),
    Assign(Assign<'a>),
    If(If<'a>),
    Goto(Goto<'a>),
    Label(Label<'a>),
    While(While<'a>),
    Return(Return<'a>),
    Continue(Continue),
    Break(Break),
}

impl fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Call(call) => write!(f, "{}", call),
            Statement::Assign(assign) => write!(f, "{}", assign),
            Statement::If(if_) => write!(f, "{}", if_),
            Statement::Goto(goto) => write!(f, "{}", goto),
            Statement::Label(label) => write!(f, "{}", label),
            Statement::While(while_) => write!(f, "{}", while_),
            Statement::Return(return_) => write!(f, "{}", return_),
            Statement::Continue(continue_) => write!(f, "{}", continue_),
            Statement::Break(break_) => write!(f, "{}", break_),
        }
    }
}

#[derive(Debug, Clone, Default, Deref, DerefMut)]
pub struct Block<'a>(pub Vec<Statement<'a>>);

impl<'a> Block<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_vec(statements: Vec<Statement<'a>>) -> Self {
        Self(statements)
    }
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|node| node.to_string()).join("\n")
        )
    }
}
