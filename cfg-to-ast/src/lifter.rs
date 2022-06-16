use std::rc::Rc;

use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{ConditionalJump, Inner, Return, Terminator},
    value::ValueId,
};
use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::{back_edges, dfs_tree, dominators::post_dominator_tree},
    NodeId,
};

fn assign_local(local: ast_ir::ExprLocal, value: ast_ir::Expr) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: vec![local.into()],
        values: vec![value],
    }
}

fn assign_locals(locals: Vec<ast_ir::ExprLocal>, values: Vec<ast_ir::Expr>) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: locals.into_iter().map(|v| v.into()).collect(),
        values,
    }
}

fn if_statement(condition: ast_ir::ExprLocal) -> ast_ir::If {
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

fn while_statement(condition: ast_ir::Expr, body: ast_ir::Block) -> ast_ir::While {
    ast_ir::While {
        pos: None,
        condition,
        body,
    }
}

fn break_statement() -> ast_ir::Break {
    ast_ir::Break { pos: None }
}

fn constant(constant: &Constant) -> ast_ir::ExprLit {
    ast_ir::ExprLit {
        pos: None,
        lit: match constant.clone() {
            Constant::Nil => ast_ir::Lit::Nil,
            Constant::Boolean(v) => ast_ir::Lit::Boolean(v),
            Constant::Number(v) => ast_ir::Lit::Number(v),
            // TODO: Cow strings?
            Constant::String(v) => ast_ir::Lit::String(v),
        },
    }
}

fn global(name: String) -> ast_ir::Global {
    ast_ir::Global { pos: None, name }
}

fn call_expression(value: ast_ir::Expr, arguments: Vec<ast_ir::Expr>) -> ast_ir::Call {
    ast_ir::Call {
        pos: None,
        arguments,
        value: Box::new(value),
        is_self: false,
    }
}

#[derive(Debug)]
enum Link {
    Extend(NodeId),
    If(Box<Link>, Option<Box<Link>>, Option<NodeId>),
    Break,
    None,
}

struct Lifter<'a> {
    function: &'a Function,
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

    fn local(&mut self, value: ValueId) -> ast_ir::ExprLocal {
        ast_ir::ExprLocal {
            pos: None,
            local: self.locals[&value].clone(),
            prefix: false,
        }
    }

    fn lift_block(&mut self, node: NodeId, is_while: bool) -> ast_ir::Block {
        let mut body = ast_ir::Block::new(None);

        let block = self.function.block(node).unwrap();

        for instruction in &block.inner_instructions {
            match instruction {
                Inner::LoadConstant(load_constant) => body.statements.push(
                    assign_local(
                        self.local(load_constant.dest),
                        constant(&load_constant.constant).into(),
                    )
                    .into(),
                ),
                Inner::LoadGlobal(load_global) => body.statements.push(
                    assign_local(
                        self.local(load_global.dest),
                        global(load_global.name.clone()).into(),
                    )
                    .into(),
                ),
                Inner::Move(mov) => body
                    .statements
                    .push(assign_local(self.local(mov.dest), self.local(mov.source).into()).into()),
                Inner::Call(call) => {
                    let function = self.local(call.function).into();
                    let return_values = call
                        .return_values
                        .iter()
                        .map(|&v| self.local(v))
                        .collect::<Vec<_>>();
                    let arguments = call
                        .arguments
                        .iter()
                        .map(|&v| self.local(v).into())
                        .collect::<Vec<_>>();
                    let call_expr = call_expression(function, arguments);
                    body.statements.push(if return_values.is_empty() {
                        call_expr.into()
                    } else {
                        assign_locals(return_values, vec![call_expr.into()]).into()
                    });
                }
                _ => {}
            }
        }

        match block.terminator() {
            Some(Terminator::UnconditionalJump { .. }) => {}
            Some(Terminator::ConditionalJump(ConditionalJump { condition, .. })) => body
                .statements
                .push(if_statement(self.local(*condition)).into()),
            Some(Terminator::NumericFor { .. }) => panic!(),
            Some(Terminator::Return(Return { values, .. })) => body.statements.push(
                return_statement(values.iter().map(|&v| self.local(v).into()).collect()).into(),
            ),
            None => panic!("block has no terminator"),
        }

        if is_while {
            let mut new_body = ast_ir::Block::new(None);
            new_body
                .statements
                .push(while_statement(constant(&Constant::Boolean(true)).into(), body).into());
            new_body
        } else {
            body
        }
    }

    fn edge(
        stack: &mut Vec<NodeId>,
        visited: &[NodeId],
        stops: &FxHashSet<NodeId>,
        loop_exits: &FxHashSet<NodeId>,
        node: NodeId,
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

    pub fn lift(&mut self, root: NodeId) -> ast_ir::Function {
        let mut ast_function = ast_ir::Function::new();

        let graph = self.function.graph();

        let loop_headers = back_edges(graph, root)
            .unwrap()
            .iter()
            .map(|edge| edge.destination)
            .collect::<FxHashSet<_>>();
        let post_dom_tree =
            post_dominator_tree(graph, root, &dfs_tree(graph, root).unwrap()).unwrap();
        let loop_exits = loop_headers
            .iter()
            .filter_map(|&n| post_dom_tree.predecessors(n).next())
            .collect::<FxHashSet<_>>();

        let mut blocks = self
            .function
            .graph()
            .nodes()
            .iter()
            .map(|&n| (n, self.lift_block(n, loop_headers.contains(&n))))
            .collect::<FxHashMap<_, _>>();

        let mut links = FxHashMap::default();

        let mut stack = vec![root];
        let mut visited = Vec::new();
        let mut stops = FxHashSet::default();

        while let Some(node) = stack.pop() {
            assert!(!visited.contains(&node));
            visited.push(node);

            //println!("visiting: {}", node);

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
                        let exit = post_dom_tree.predecessors(node).next();
                        if let Some(exit) = exit {
                            assert!(successors[0] != successors[1]);
                            stack.push(exit);
                            stops.insert(exit);

                            if !loop_exits.contains(&exit) {
                                if successors[0] == exit {
                                    successors.swap(0, 1);
                                }
                                 if successors[1] == exit {
                                    has_else = false;
                                }
                            }
                        }
                        Link::If(
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
                        )
                    }
                    _ => panic!("too many successors"),
                },
            );
        }

        for (node, link) in visited.iter().rev().map(|&n| (n, &links[&n])) {
            match link {
                Link::If(true_branch, false_branch, exit) => {
                    let then_statements = match **true_branch {
                        Link::Extend(target) => {
                            blocks.remove(&target).unwrap().statements
                        }
                        Link::Break => vec![break_statement().into()],
                        _ => panic!(),
                    };
                    let else_statements = false_branch.as_ref().map(|link| match **link {
                        Link::Extend(target) => blocks.remove(&target).unwrap().statements,
                        Link::Break => vec![break_statement().into()],
                        _ => panic!(),
                    });
                    let if_stat = {
                        let statement = blocks
                            .get_mut(&node)
                            .unwrap()
                            .statements
                            .last_mut()
                            .unwrap();
                        if let Some(if_stat) = statement.as_if_mut() {
                            if_stat
                        } else {
                            statement
                                .as_while_mut()
                                .unwrap()
                                .body
                                .statements
                                .first_mut()
                                .unwrap()
                                .as_if_mut()
                                .unwrap()
                        }
                    };
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
                        let block = blocks.remove(exit).unwrap();
                        blocks
                            .get_mut(&node)
                            .unwrap()
                            .statements
                            .extend(block.statements.into_iter());
                    }
                }
                Link::Extend(target) => {
                    let block = blocks.remove(target).unwrap();
                    blocks
                        .get_mut(&node)
                        .unwrap()
                        .statements
                        .extend(block.statements.into_iter());
                }
                Link::Break => blocks
                    .get_mut(&node)
                    .unwrap()
                    .statements
                    .push(break_statement().into()),
                _ => {}
            }
        }

        ast_function.body = blocks.remove(&root).unwrap();
        println!("{}", ast_ir::formatter::format_ast(&ast_function));

        ast_function
    }
}

pub fn lift(function: &Function) {
    let entry = function.entry().unwrap();
    let graph = function.graph();

    let mut lifter = Lifter::new(function);
    let ast_function = lifter.lift(entry);
}
