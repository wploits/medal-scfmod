use std::{collections::BTreeSet, rc::Rc};

use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{ConditionalJump, Inner, Terminator},
    value::ValueId,
};
use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::{dfs_tree, dominators::post_dominator_tree},
    NodeId,
};

fn assign_local(local: ast_ir::ExprLocal, value: ast_ir::Expr) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: vec![local.into()],
        values: vec![value],
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

fn return_statement() -> ast_ir::Return {
    ast_ir::Return {
        pos: None,
        values: Vec::new(),
    }
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

#[derive(Debug)]
enum Link {
    Extend(NodeId),
    If(Box<Link>, Option<Box<Link>>, Option<NodeId>),
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

    fn lift_block(&mut self, node: NodeId) -> ast_ir::Block {
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
                Inner::Move(mov) => body
                    .statements
                    .push(assign_local(self.local(mov.dest), self.local(mov.source).into()).into()),
                _ => {}
            }
        }

        match block.terminator() {
            Some(Terminator::UnconditionalJump { .. }) => {}
            Some(Terminator::ConditionalJump(ConditionalJump { condition, .. })) => body
                .statements
                .push(if_statement(self.local(*condition)).into()),
            Some(Terminator::NumericFor { .. }) => panic!(),
            Some(Terminator::Return { .. }) => body.statements.push(return_statement().into()),
            None => panic!("block has no terminator"),
        }

        body
    }

    fn edge(
        stack: &mut Vec<NodeId>,
        visited: &mut Vec<NodeId>,
        stops: &FxHashSet<NodeId>,
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
        } else {
            Link::None
        }
    }

    pub fn lift(&mut self, root: NodeId) -> ast_ir::Function {
        let mut ast_function = ast_ir::Function::new();

        let graph = self.function.graph();
        let post_dom_tree =
            post_dominator_tree(graph, root, &dfs_tree(graph, root).unwrap()).unwrap();

        let mut blocks = self
            .function
            .graph()
            .nodes()
            .iter()
            .map(|&n| (n, self.lift_block(n)))
            .collect::<FxHashMap<_, _>>();

        let mut links = FxHashMap::default();

        let mut stack = vec![root];
        let mut visited = Vec::new();
        let mut stops = FxHashSet::default();

        while let Some(node) = stack.pop() {
            assert!(!visited.contains(&node));
            visited.push(node);

            //println!("visiting: {}", node);

            let successors = self.function.graph().successors(node).collect::<Vec<_>>();
            links.insert(
                node,
                match successors.len() {
                    0 => Link::None,
                    1 => Self::edge(&mut stack, &mut visited, &stops, node, successors[0]),
                    2 => {
                        if let Some(exit) = post_dom_tree.predecessors(node).next() {
                            assert!(!visited.contains(&exit));
                            stack.push(exit);
                            stops.insert(exit);
                            Link::If(
                                Box::new(Self::edge(
                                    &mut stack,
                                    &mut visited,
                                    &stops,
                                    node,
                                    successors[0],
                                )),
                                Some(Box::new(Self::edge(
                                    &mut stack,
                                    &mut visited,
                                    &stops,
                                    node,
                                    successors[1],
                                ))),
                                Some(exit),
                            )
                        } else {
                            todo!()
                        }
                    }
                    _ => panic!("too many successors"),
                },
            );
        }

        for (node, link) in visited.iter().rev().map(|&n| (n, &links[&n])) {
            println!("{}", node);
            match link {
                Link::If(true_branch, false_branch, exit) => {
                    let then_statements = match **true_branch {
                        Link::Extend(target) => {
                            blocks.remove(&target).unwrap().statements.into_iter()
                        }
                        _ => panic!(),
                    };
                    let else_statements = false_branch.as_ref().map(|link| match **link {
                        Link::Extend(target) => blocks.remove(&target).unwrap().statements,
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
                        if_stat
                            .else_block
                            .as_mut()
                            .unwrap()
                            .statements
                            .extend(else_statements.into_iter());
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
