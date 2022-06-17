use std::rc::Rc;

use super::{
    expr::{Call, Closure, Expr},
    Local, Pos,
};

use derivative::Derivative;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Stat<'ast> {
    Block(Block<'ast>),
    Assign(Assign<'ast>),
    Break(Break),
    Continue(Continue),
    Return(Return<'ast>),
    If(If<'ast>),
    FuncDef(FuncDef<'ast>),
    LocalDef(LocalDef<'ast>),
    LocalFuncDef(LocalFuncDef<'ast>),
    Call(Call<'ast>),
    While(While<'ast>),
    NumericFor(NumericFor<'ast>),
    IterativeFor(IterativeFor<'ast>),
    Repeat(Repeat<'ast>),
    Comment(Comment),
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Comment {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub comment: String,
}

impl_node_no_lifetime!(Comment);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Block<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub statements: Vec<Stat<'ast>>,
}

impl_node!(Block);

impl<'ast> Block<'ast> {
    pub fn new(pos: Option<Pos>) -> Self {
        Self {
            pos,
            statements: Vec::new(),
        }
    }

    pub fn with_statements(pos: Option<Pos>, statements: Vec<Stat<'ast>>) -> Self {
        Self { pos, statements }
    }
}

impl<'ast> Default for Block<'ast> {
    fn default() -> Self {
        Self::new(None)
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Assign<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub vars: Vec<Expr<'ast>>,
    pub values: Vec<Expr<'ast>>,
    pub local_prefix: bool,
}

impl_node!(Assign);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Break {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
}

impl_node_no_lifetime!(Break);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Continue {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
}

impl_node_no_lifetime!(Continue);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Return<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub values: Vec<Expr<'ast>>,
}

impl_node!(Return);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct If<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub condition: Expr<'ast>,
    // TODO: we call it 'then_block' here but 'body' elsewhere?
    pub then_block: Block<'ast>,
    pub else_block: Option<Block<'ast>>,
}

impl_node!(If);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct FuncDef<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub expr: Expr<'ast>,
    pub func_expr: Closure<'ast>,
}

impl_node!(FuncDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct LocalDef<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub locals: Vec<Rc<Local>>,
    pub values: Vec<Expr<'ast>>,
}

impl_node!(LocalDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct LocalFuncDef<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub local: Rc<Local>,
    pub func_expr: Closure<'ast>,
}

impl_node!(LocalFuncDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct While<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub condition: Expr<'ast>,
    pub body: Block<'ast>,
}

impl_node!(While);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct NumericFor<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub var: Rc<Local>,

    pub from: Expr<'ast>,
    pub to: Expr<'ast>,
    pub step: Option<Expr<'ast>>,

    pub body: Block<'ast>,
}

impl_node!(NumericFor);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct IterativeFor<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub vars: Vec<Rc<Local>>,
    pub values: Vec<Expr<'ast>>,
    pub body: Block<'ast>,
}

impl_node!(IterativeFor);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Repeat<'ast> {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub body: Block<'ast>,
    pub cond: Expr<'ast>,
}

impl_node!(Repeat);
