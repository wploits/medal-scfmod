pub mod location;

pub(crate) mod branch_info;
pub(crate) mod value_info;

mod phi;
mod terminator;

mod binary;
mod unary;

mod call;
mod concat;

mod load;
mod r#move;
mod store;

pub use phi::Phi;
pub use terminator::{ConditionalJump, Return, Terminator, UnconditionalJump};

pub use binary::{Binary, BinaryOp};
pub use unary::{Unary, UnaryOp};

pub use call::Call;
pub use concat::Concat;

pub use load::{LoadConstant, LoadGlobal, LoadIndex};
pub use r#move::Move;
pub use store::{StoreGlobal, StoreIndex};

use super::value::ValueId;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use value_info::ValueInfo;

/// A struct that represents an instruction in the IR that is not a terminator or phi.
#[enum_dispatch(ValueInfo)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum Instruction {
    Binary(Binary),
    Unary(Unary),
    LoadConstant(LoadConstant),
    LoadGlobal(LoadGlobal),
    LoadIndex(LoadIndex),
    Move(Move),
    StoreGlobal(StoreGlobal),
    StoreIndex(StoreIndex),
    Concat(Concat),
    Call(Call),
}
