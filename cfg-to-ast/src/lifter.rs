use std::{rc::Rc, borrow::Cow};

use fxhash::{FxHashMap, FxHashSet};

use ast_ir::Expr;
use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{
        BinaryOp, ConditionalJump, Inner, location::InstructionIndex, Return, Terminator, UnaryOp,
    },
    value::ValueId,
};
use graph::{
    algorithms::{
        back_edges, dfs_tree,
        dominators::{compute_immediate_dominators, post_dominator_tree},
    },
    NodeId,
};
use local_declaration::Declaration;

mod local_declaration;
mod optimizer;

fn assign<'a>(variable: ast_ir::Expr<'a>, values: Vec<Expr<'a>>) -> ast_ir::Assign<'a> {
    ast_ir::Assign {
        pos: None,
        vars: vec![variable],
        values,
        local_prefix: false,
    }
}

fn assign_local(
    local: ast_ir::ExprLocal,
    value: ast_ir::Expr,
    local_prefix: bool,
) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: vec![local.into()],
        values: vec![value],
        local_prefix,
    }
}

fn assign_locals(locals: Vec<ast_ir::ExprLocal>, values: Vec<ast_ir::Expr>) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: locals.into_iter().map(|v| v.into()).collect(),
        values,
        local_prefix: false,
    }
}

fn if_statement<'a>(condition: ast_ir::ExprLocal) -> ast_ir::If<'a> {
    ast_ir::If {
        pos: None,
        condition: condition.into(),
        then_block: ast_ir::Block::new(None),
        else_block: Some(ast_ir::Block::new(None)),
    }
}

fn return_statement(values: Vec<ast_ir::Expr>) -> ast_ir::Return {
    ast_ir::Return { pos: None, values }
}

fn while_statement<'a>(condition: ast_ir::Expr<'a>, body: ast_ir::Block<'a>) -> ast_ir::While<'a> {
    ast_ir::While {
        pos: None,
        condition,
        body,
    }
}

fn break_statement() -> ast_ir::Break {
    ast_ir::Break { pos: None }
}

fn constant<'a>(constant: &'a Constant) -> ast_ir::ExprLit<'a> {
    ast_ir::ExprLit {
        pos: None,
        lit: match constant {
            Constant::Nil => ast_ir::Lit::Nil,
            Constant::Boolean(v) => ast_ir::Lit::Boolean(*v),
            Constant::Number(v) => ast_ir::Lit::Number(*v),
            Constant::String(v) => ast_ir::Lit::String(Cow::Borrowed(v)),
        },
    }
}

fn global(name: &str) -> ast_ir::Global {
    ast_ir::Global { pos: None, name: Cow::Borrowed(name) }
}

fn call_expression<'a>(value: ast_ir::Expr<'a>, arguments: Vec<ast_ir::Expr<'a>>) -> ast_ir::Call<'a> {
    ast_ir::Call {
        pos: None,
        arguments,
        value: Box::new(value),
        is_self: false,
    }
}

fn binary_expression<'a>(
    left: ast_ir::Expr<'a>,
    right: ast_ir::Expr<'a>,
    op: ast_ir::BinaryOp,
) -> ast_ir::Binary<'a> {
    ast_ir::Binary {
        pos: None,
        lhs: Box::new(left),
        rhs: Box::new(right),
        op,
    }
}

fn binary_expression_fold(mut v: Vec<Expr>, op: ast_ir::BinaryOp) -> Expr {
    fn _binary_expression_fold<'a>(mut v: Vec<Expr<'a>>, lhs: Expr<'a>, op: ast_ir::BinaryOp) -> Expr<'a> {
        if v.is_empty() {
            return lhs;
        }

        let rhs = Box::new(v.remove(0));

        _binary_expression_fold(
            v,
            ast_ir::Binary {
                pos: None,
                op,
                lhs: Box::new(lhs),
                rhs,
            }
                .into(),
            op,
        )
    }

    let lhs = v.remove(0);

    _binary_expression_fold(v, lhs, op)
}

fn unary_expression(value: Expr, op: ast_ir::UnaryOp) -> ast_ir::Unary {
    ast_ir::Unary {
        pos: None,
        op,
        expr: Box::new(value),
    }
}

#[derive(Debug)]
enum Link {
    Extend(NodeId),
    If(Box<Link>, Option<Box<Link>>, Option<NodeId>),
    Break,
    None,
}

#[derive(Debug)]
enum Loop {
    While(NodeId),
}

struct Lifter<'a> {
    function: &'a Function<'a>,
    locals: FxHashMap<ValueId, Rc<ast_ir::Local>>,
}

impl<'a> Lifter<'a> {
    pub fn new(function: &'a Function) -> Self {
        Self {
            function,
            locals: function
                .values()
                .iter()
                .map(|&v| {
                    (
                        v,
                        Rc::new(ast_ir::Local {
                            name: v.to_string(),
                        }),
                    )
                })
                .collect::<FxHashMap<_, _>>(),
        }
    }

    fn local(&self, value: &ValueId) -> ast_ir::ExprLocal {
        ast_ir::ExprLocal {
            pos: None,
            local: self.locals[value].clone(),
        }
    }

    fn handle_instruction_declarations(
        &self,
        index: InstructionIndex,
        local_declarations: &FxHashMap<InstructionIndex, &Vec<Declaration>>,
        body: &mut ast_ir::Block,
    ) -> FxHashSet<ValueId> {
        if let Some(declarations) = local_declarations.get(&index) {
            let forward_declarations = declarations
                .iter()
                .filter_map(|declaration| match declaration {
                    Declaration::Forward(value) if !self.function.parameters.contains(value) => {
                        Some(value)
                    }
                    _ => None,
                })
                .collect::<FxHashSet<_>>();
            for value in forward_declarations {
                body.statements.push(
                    assign_local(self.local(&value), constant(&Constant::Nil).into(), true).into(),
                )
            }
            declarations
                .iter()
                .filter_map(|declaration| {
                    if let &Declaration::Inline(value) = declaration {
                        Some(value)
                    } else {
                        None
                    }
                })
                .collect::<FxHashSet<_>>()
        } else {
            FxHashSet::default()
        }
    }

    fn lift_closure(&self, function: &'a cfg_ir::function::Function) -> ast_ir::Closure<'a> {
        let args = function
            .parameters
            .iter()
            .map(|v| {
                Rc::new(ast_ir::Local {
                    name: v.to_string(),
                })
            })
            .collect();

        ast_ir::Closure {
            pos: None,
            self_: None,
            args,
            block: lift(function).body,
        }
    }

    fn lift_block(
        &mut self,
        node: NodeId,
        local_declarations: FxHashMap<InstructionIndex, &Vec<Declaration>>,
    ) -> ast_ir::Block<'a> {
        let mut body = ast_ir::Block::new(None);
        let block = self.function.block(node).unwrap();
        let mut variadic_expr = None;
        for (index, instruction) in block.inner_instructions.iter().enumerate() {
            let local_prefixes = self.handle_instruction_declarations(
                InstructionIndex::Inner(index),
                &local_declarations,
                &mut body,
            );

            let assign_local = |value, expr| {
                assign_local(self.local(value), expr, local_prefixes.contains(value))
            };

            match instruction {
                Inner::LoadConstant(load_constant) => body.statements.push(
                    assign_local(
                        &load_constant.dest,
                        constant(&load_constant.constant).into(),
                    )
                        .into(),
                ),
                Inner::Binary(binary) => body.statements.push(
                    assign_local(
                        &binary.dest,
                        binary_expression(
                            self.local(&binary.lhs).into(),
                            self.local(&binary.rhs).into(),
                            match binary.op {
                                BinaryOp::Add => ast_ir::BinaryOp::Add,
                                BinaryOp::Sub => ast_ir::BinaryOp::Sub,
                                BinaryOp::Mul => ast_ir::BinaryOp::Mul,
                                BinaryOp::Div => ast_ir::BinaryOp::Div,
                                BinaryOp::Mod => ast_ir::BinaryOp::Mod,
                                BinaryOp::Pow => ast_ir::BinaryOp::Pow,
                                BinaryOp::Equal => ast_ir::BinaryOp::Equal,
                                BinaryOp::LesserOrEqual => ast_ir::BinaryOp::LesserOrEqual,
                                BinaryOp::LesserThan => ast_ir::BinaryOp::LesserThan,
                                BinaryOp::LogicalAnd => ast_ir::BinaryOp::LogicalAnd,
                                BinaryOp::LogicalOr => ast_ir::BinaryOp::LogicalOr,
                            },
                        )
                            .into(),
                    )
                        .into(),
                ),
                Inner::Unary(unary) => body.statements.push(
                    assign_local(
                        &unary.dest,
                        unary_expression(
                            self.local(&unary.value).into(),
                            match unary.op {
                                UnaryOp::Minus => ast_ir::UnaryOp::Minus,
                                UnaryOp::LogicalNot => ast_ir::UnaryOp::LogicalNot,
                                UnaryOp::Len => ast_ir::UnaryOp::Len,
                            },
                        ).into(),
                    ).into()
                ),
                Inner::LoadGlobal(load_global) => body.statements.push(
                    assign_local(&load_global.dest, global(&load_global.name).into()).into(),
                ),
                Inner::LoadIndex(load_index) => body.statements.push(
                    assign_local(
                        &load_index.dest,
                        Expr::Index(
                            ast_ir::Index {
                                pos: None,
                                expr: Box::new(self.local(&load_index.object).into()),
                                indices: vec![self.local(&load_index.key).into()],
                            }
                        ),
                    ).into()
                ),
                Inner::Move(mov) => body
                    .statements
                    .push(assign_local(&mov.dest, self.local(&mov.source).into()).into()),
                Inner::StoreGlobal(store_global) => body.statements.push(
                    assign(
                        ast_ir::Global {
                            pos: None,
                            name: store_global.name.clone(),
                        }.into(),
                        vec![self.local(&store_global.value).into()],
                    ).into()
                ),
                Inner::StoreIndex(store_index) => body.statements.push(
                    assign(
                        ast_ir::Index {
                            pos: None,
                            expr: Box::new(self.local(&store_index.object).into()),
                            indices: vec![self.local(&store_index.key).into()],
                        }.into(),
                        vec![self.local(&store_index.value).into()]
                    ).into()
                ),
                Inner::Call(call) => {
                    let function = self.local(&call.function).into();
                    let return_values = call
                        .return_values
                        .iter()
                        .map(|v| self.local(v))
                        .collect::<Vec<_>>();
                    let mut arguments = call
                        .arguments
                        .iter()
                        .map(|v| self.local(v).into())
                        .collect::<Vec<_>>();
                    if call.variadic_arguments {
                        assert!(variadic_expr.is_some());
                        arguments.push(variadic_expr.take().unwrap());
                    }
                    let call_expr = call_expression(function, arguments);
                    if call.variadic_return {
                        variadic_expr = Some(call_expr.into());
                    } else {
                        body.statements.push(if return_values.is_empty() {
                            call_expr.into()
                        } else {
                            assign_locals(return_values, vec![call_expr.into()]).into()
                        });
                    }
                }
                Inner::Concat(concat) => {
                    let operands: Vec<_> =
                        concat.values.iter().map(|v| self.local(v).into()).collect();

                    body.statements.push(
                        assign_local(
                            &concat.dest,
                            binary_expression_fold(operands, ast_ir::BinaryOp::Concat),
                        )
                            .into(),
                    );
                }
                Inner::Closure(closure) => {
                    let dest = &closure.dest;
                    let closure = self.lift_closure(closure.function.as_ref()).into();

                    body.statements.push(assign_local(dest, closure).into());
                }
                _ => {}
            }
        }

        match block.terminator() {
            Some(Terminator::UnconditionalJump { .. }) => {}
            Some(Terminator::ConditionalJump(ConditionalJump { condition, .. })) => body
                .statements
                .push(if_statement(self.local(condition)).into()),
            Some(Terminator::NumericForEnter { .. }) => panic!(),
            Some(Terminator::NumericForLoop { .. }) => panic!(),
            Some(Terminator::Return(return_stat)) => {
                let mut return_values = return_stat.values.iter().map(|v| self.local(v).into()).collect::<Vec<_>>();
                if return_stat.variadic {
                    assert!(variadic_expr.is_some());
                    return_values.push(variadic_expr.take().unwrap());
                }
                body.statements.push(
                    return_statement(return_values).into(),
                );
            }
            None => panic!("block has no terminator"),
        }

        body
    }

    fn edge(
        stack: &mut Vec<NodeId>,
        visited: &[NodeId],
        stops: &FxHashSet<NodeId>,
        loop_exits: &FxHashSet<NodeId>,
        _node: NodeId,
        target: NodeId,
    ) -> Link {
        if !stops.contains(&target) {
            if !visited.contains(&target) {
                stack.push(target);
                Link::Extend(target)
            } else {
                Link::None
            }
        } else if loop_exits.contains(&target) {
            Link::Break
        } else {
            Link::None
        }
    }

    pub fn lift(&mut self, root: NodeId) -> ast_ir::Function<'a> {
        let mut ast_function = ast_ir::Function::new();

        let graph = self.function.graph();

        let loop_headers = back_edges(graph, root)
            .unwrap()
            .iter()
            .map(|edge| edge.destination)
            .collect::<FxHashSet<_>>();

        let dfs = dfs_tree(graph, root).unwrap();

        let post_dom_tree = post_dominator_tree(graph, root, &dfs).unwrap();
        let loop_exits = loop_headers
            .iter()
            .filter_map(|&n| post_dom_tree.predecessors(n).next())
            .collect::<FxHashSet<_>>();

        let idoms = compute_immediate_dominators(graph, root, &dfs).unwrap();

        let local_declarations = local_declaration::local_declarations(self.function, root, &idoms);

        let mut blocks = self
            .function
            .graph()
            .nodes()
            .iter()
            .map(|&n| {
                (
                    n,
                    self.lift_block(
                        n,
                        local_declarations
                            .iter()
                            .filter_map(|(location, declarations)| {
                                if location.node == n {
                                    Some((location.index, declarations))
                                } else {
                                    None
                                }
                            })
                            .collect(),
                    ),
                )
            })
            .collect::<FxHashMap<_, _>>();

        let mut links = FxHashMap::default();
        let mut loops = FxHashMap::default();

        let mut stack = vec![root];
        let mut visited = Vec::new();
        let mut stops = FxHashSet::default();

        while let Some(node) = stack.pop() {
            //println!("visiting: {}", node);
            assert!(!visited.contains(&node));
            visited.push(node);

            // todo: for loops
            if loop_headers.contains(&node) {
                if let Some(loop_exit) = post_dom_tree.predecessors(node).next() {
                    assert!(!visited.contains(&loop_exit));
                    loops.insert(node, Loop::While(loop_exit));
                    stops.insert(loop_exit);
                    stack.push(loop_exit);
                }
            }

            let mut successors = self.function.graph().successors(node).collect::<Vec<_>>();
            links.insert(
                node,
                match successors.len() {
                    0 => Link::None,
                    1 => Self::edge(
                        &mut stack,
                        &visited,
                        &stops,
                        &loop_exits,
                        node,
                        successors[0],
                    ),
                    2 => {
                        let mut has_else = true;
                        let mut exit = post_dom_tree.predecessors(node).next();
                        if let Some(exit_node) = exit {
                            assert!(successors[0] != successors[1]);
                            if !stops.contains(&exit_node) && !visited.contains(&exit_node) {
                                stops.insert(exit_node);
                                if !loop_exits.contains(&exit_node) {
                                    if successors[0] == exit_node {
                                        successors.swap(0, 1);
                                    }
                                    if successors[1] == exit_node {
                                        has_else = false;
                                    }
                                }
                            } else {
                                exit = None;
                            }
                        }
                        let link = Link::If(
                            Box::new(Self::edge(
                                &mut stack,
                                &visited,
                                &stops,
                                &loop_exits,
                                node,
                                successors[0],
                            )),
                            if has_else {
                                Some(Box::new(Self::edge(
                                    &mut stack,
                                    &visited,
                                    &stops,
                                    &loop_exits,
                                    node,
                                    successors[1],
                                )))
                            } else {
                                None
                            },
                            exit,
                        );
                        if let Some(exit) = exit {
                            if !visited.contains(&exit) {
                                stack.push(exit);
                            }
                        }
                        link
                    }
                    _ => panic!("too many successors"),
                },
            );
        }

        println!("{:#?}", links);

        fn build_link(
            node: NodeId,
            link: &Link,
            blocks: &mut FxHashMap<NodeId, ast_ir::Block>,
            loops: &FxHashMap<NodeId, Loop>,
        ) {
            match link {
                Link::If(then_link, else_link, exit) => {
                    let then_statements = match **then_link {
                        Link::Extend(target) => blocks.remove(&target).unwrap().statements,
                        Link::Break => vec![break_statement().into()],
                        _ => panic!(),
                    };
                    let else_statements = else_link.as_ref().map(|link| match **link {
                        Link::Extend(target) => blocks.remove(&target).unwrap().statements,
                        Link::Break => vec![break_statement().into()],
                        _ => panic!(),
                    });
                    let if_stat = blocks
                        .get_mut(&node)
                        .unwrap()
                        .statements
                        .last_mut()
                        .unwrap()
                        .as_if_mut()
                        .unwrap();
                    if_stat
                        .then_block
                        .statements
                        .extend(then_statements.into_iter());
                    if let Some(else_statements) = else_statements {
                        if else_statements.is_empty() {
                            if_stat.else_block = None;
                        } else {
                            if_stat
                                .else_block
                                .as_mut()
                                .unwrap()
                                .statements
                                .extend(else_statements.into_iter());
                        }
                    }
                    if let Some(exit) = exit {
                        if let Some(block) = blocks.remove(exit) {
                            blocks
                                .get_mut(&node)
                                .unwrap()
                                .statements
                                .extend(block.statements.into_iter());
                        }
                    }
                }
                &Link::Break => blocks
                    .get_mut(&node)
                    .unwrap()
                    .statements
                    .push(break_statement().into()),
                Link::Extend(target) => {
                    let block = blocks.remove(target).unwrap();
                    blocks
                        .get_mut(&node)
                        .unwrap()
                        .statements
                        .extend(block.statements.into_iter())
                }
                Link::None => {}
            }
            if let Some(_loop) = loops.get(&node) {
                match _loop {
                    Loop::While(loop_exit) => {
                        let mut new_block = ast_ir::Block::new(None);
                        let mut while_stat = while_statement(
                            constant(&Constant::Boolean(true)).into(),
                            blocks.remove(&node).unwrap(),
                        );
                        optimizer::optimize_while(&mut while_stat);
                        new_block.statements.push(while_stat.into());
                        new_block
                            .statements
                            .extend(blocks.remove(loop_exit).unwrap().statements.into_iter());
                        blocks.insert(node, new_block);
                    }
                }
            }
        }

        for (node, link) in visited.iter().rev().map(|&n| (n, &links[&n])) {
            build_link(node, link, &mut blocks, &loops);
        }

        ast_function.body = blocks.remove(&root).unwrap();
        ast_function
    }
}

pub fn lift<'a>(function: &'a Function) -> ast_ir::Function<'a> {
    let entry = function.entry().unwrap();
    let mut lifter = Lifter::new(function);
    lifter.lift(entry)
}
