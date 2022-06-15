use std::{collections::HashSet, rc::Rc};

use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{ConditionalJump, Inner, Terminator},
    value::ValueId,
};
use fxhash::FxHashMap;
use graph::{algorithms::dominators::post_dominator_tree, NodeId};

fn assign_local(local: ast_ir::ExprLocal, value: ast_ir::Expr) -> ast_ir::Assign {
    ast_ir::Assign {
        pos: None,
        vars: vec![local.into()],
        values: vec![value.into()],
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
            Constant::String(v) => ast_ir::Lit::String(v),
        },
    }
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
                        self.local(load_constant.dest).into(),
                        constant(&load_constant.constant).into(),
                    )
                    .into(),
                ),
                Inner::Move(mov) => body.statements.push(
                    assign_local(self.local(mov.dest).into(), self.local(mov.source).into()).into(),
                ),
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

    pub fn lift(&mut self, root: NodeId) -> ast_ir::Function {
        let mut ast_function = ast_ir::Function::new();

        let blocks = self
            .function
            .graph()
            .nodes()
            .iter()
            .map(|&n| (n, self.lift_block(n)))
            .collect::<FxHashMap<_, _>>();

        println!("{:#?}", blocks);

        ast_function
    }
}

pub fn lift(function: &Function) {
    let entry = function.entry().unwrap();
    let graph = function.graph();

    let mut lifter = Lifter::new(function);
    let ast_function = lifter.lift(entry);

    //println!("{}", ast_ir::formatter::format_ast(&ast_function));

    /*let post_dom_tree = post_dominator_tree(graph, entry).unwrap();

    let mut visited = HashSet::new();
    let mut stack = vec![entry];
    while let Some(node) = stack.pop() {
        if !visited.insert(node) {
            continue;
        }
        println!("visiting: {}", node);

        let successors = graph.successors(node).collect::<Vec<_>>();

        // Conditional
        if successors.len() == 2 {
            let exit = post_dom_tree.predecessors(node).next();
            if let Some(exit) = exit {
                for next in successors {
                    stack.push(next);
                }
                stack.push(exit);
                continue;
            }

            println!("exit: {:?}", exit);
        } else {
            for next in successors {
                stack.push(next);
            }
        }
    }*/
}
