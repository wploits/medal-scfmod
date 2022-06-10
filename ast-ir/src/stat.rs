use std::rc::Rc;

use super::{
    expr::{Call, Closure, Expr},
    Local, Pos,
};

use derivative::Derivative;
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
#[derive(Debug, PartialEq, Clone)]
pub enum Stat {
    Block(Block),
    Assign(Assign),
    Break(Break),
    Continue(Continue),
    Return(Return),
    If(If),
    FuncDef(FuncDef),
    LocalDef(LocalDef),
    LocalFuncDef(LocalFuncDef),
    Call(Call),
    While(While),
    NumericFor(NumericFor),
    IterativeFor(IterativeFor),
    Repeat(Repeat),
    Comment(Comment),
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Comment {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub comment: String,
}

impl_node!(Comment);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Block {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub statements: Vec<Stat>,
}

impl_node!(Block);

impl Block {
    pub fn new(pos: Option<Pos>) -> Self {
        Self {
            pos,
            statements: Vec::<Stat>::new(),
        }
    }

    pub fn with_statements(pos: Option<Pos>, statements: Vec<Stat>) -> Self {
        Self { pos, statements }
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new(None)
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Assign {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub vars: Vec<Expr>,
    pub values: Vec<Expr>,
}

impl_node!(Assign);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Break {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
}

impl_node!(Break);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Continue {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
}

impl_node!(Continue);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Return {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub values: Vec<Expr>,
}

impl_node!(Return);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct If {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub condition: Expr,
    // TODO: then_block here but body elsewhere?
    pub then_block: Block,
    pub else_block: Option<Block>,
}

impl_node!(If);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct FuncDef {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub expr: Expr,
    pub func_expr: Closure,
}

impl_node!(FuncDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct LocalDef {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub locals: Vec<Rc<Local>>,
    pub values: Vec<Expr>,
}

impl_node!(LocalDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct LocalFuncDef {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,
    pub local: Rc<Local>,
    pub func_expr: Closure,
}

impl_node!(LocalFuncDef);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct While {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub cond: Expr,
    pub body: Block,
}

impl_node!(While);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct NumericFor {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub var: Rc<Local>,

    pub from: Expr,
    pub to: Expr,
    pub step: Option<Expr>,

    pub body: Block,
}

impl_node!(NumericFor);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct IterativeFor {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub vars: Vec<Rc<Local>>,
    pub values: Vec<Expr>,
    pub body: Block,
}

impl_node!(IterativeFor);

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct Repeat {
    #[derivative(PartialEq = "ignore")]
    pub pos: Option<Pos>,

    pub body: Block,
    pub cond: Expr,
}

impl_node!(Repeat);
