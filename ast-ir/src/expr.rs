use std::rc::Rc;

use super::{stat::Block, Local, Pos};

use derivative::Derivative;
use enum_dispatch::enum_dispatch;

macro_rules! impl_expr {
    ($node:ident, $side_effects:expr) => {
        impl super::Node for $node {
            fn pos(&self) -> &Option<Pos> {
                &self.pos
            }
        }

        impl ExprTrait for $node {
            fn has_side_effects(&self) -> bool {
                $side_effects
            }
        }
    };
}

macro_rules! impl_expr_side_effects {
    ($node:ident, $side_effects:expr) => {
        impl super::Node for $node {
            fn pos(&self) -> &Option<Pos> {
                &self.pos
            }
        }

        impl ExprTrait for $node {
            #[allow(clippy::redundant_closure_call)]
            fn has_side_effects(&self) -> bool {
                ($side_effects)(self)
            }
        }
    };
}

#[enum_dispatch]
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Call(Call),
    Lit(ExprLit),
    Varargs(Varargs),
    Group(Group),
    Index(Index),
    Local(ExprLocal),
    Global(Global),
    Table(Table),
    Closure(Closure),
    Unary(Unary),
    Binary(Binary),
}

#[enum_dispatch(Expr)]
pub trait ExprTrait {
    fn has_side_effects(&self) -> bool;
    // TODO: fn is_constant(&self) -> bool // Can type be folded to Expr::Constant?
    // TODO: fn fold(&self) -> Expr; // no-op if self is Expr::Constant, otherwise fold to Expr::Constant
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Call {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub value: Box<Expr>,
    pub arguments: Vec<Expr>,
    pub is_self: bool,
}

impl_expr!(Call, true);

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct ExprLit {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub lit: Lit,
}

impl_expr!(ExprLit, false);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Varargs {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
}

impl_expr!(Varargs, false);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Group {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub expr: Box<Expr>,
}

impl_expr_side_effects!(Group, |e: &Group| { e.expr.has_side_effects() });

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Index {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub expr: Box<Expr>,
    pub indices: Vec<Expr>,
}

impl_expr!(Index, true);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct ExprLocal {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub local: Rc<Local>,
}

impl_expr!(ExprLocal, false);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Global {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub name: String,
}

impl_expr!(Global, true);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Table {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    // key, value
    pub pairs: Vec<(Option<Expr>, Expr)>,
}

// TODO: side effects
impl_expr!(Table, true);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Closure {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub self_: Option<Rc<Local>>,
    pub args: Vec<Rc<Local>>,
    pub block: Block,
}

impl_expr!(Closure, false);

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    LogicalNot,
    Minus,
    Len,
}

pub const fn get_unary_priority() -> usize {
    8
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Unary {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

impl_expr_side_effects!(Unary, |e: &Unary| { e.expr.has_side_effects() });

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Concat,

    Equal,
    NotEqual,

    LesserOrEqual,
    GreaterOrEqual,
    LesserThan,
    GreaterThan,

    LogicalAnd,
    LogicalOr,
}

pub fn get_binary_priority(binary_op: BinaryOp) -> (usize, usize) {
    match binary_op {
        BinaryOp::Add => (6, 6),
        BinaryOp::Sub => (6, 6),

        BinaryOp::Mul => (7, 7),
        BinaryOp::Div => (7, 7),
        BinaryOp::Mod => (7, 7),

        // right associative
        BinaryOp::Pow => (10, 9),
        BinaryOp::Concat => (5, 4),

        BinaryOp::Equal => (3, 3),
        BinaryOp::NotEqual => (3, 3),
        BinaryOp::LesserOrEqual => (3, 3),
        BinaryOp::GreaterOrEqual => (3, 3),
        BinaryOp::LesserThan => (3, 3),
        BinaryOp::GreaterThan => (3, 3),

        BinaryOp::LogicalAnd => (2, 2),
        BinaryOp::LogicalOr => (1, 1),
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Binary {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub op: BinaryOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl_expr_side_effects!(Binary, |e: &Binary| {
    e.lhs.has_side_effects() || e.rhs.has_side_effects()
});
