use enum_dispatch::enum_dispatch;

macro_rules! impl_node {
    ($node:ident) => {
        impl super::Node for $node {
            fn pos(&self) -> &Option<Pos> {
                &self.pos
            }
        }
    };
}

pub(crate) mod func;
pub use func::*;

pub(crate) mod local;
pub use local::*;

pub(crate) mod pos;
pub use pos::*;

pub(crate) mod expr;

pub(crate) mod stat;

pub mod formatter;

pub use expr::*;
pub use stat::*;

#[enum_dispatch(Expr, Stat)]
pub trait Node {
    fn pos(&self) -> &Option<Pos>;
}
