use fxhash::FxHashSet;
use graph::algorithms::dominators::post_dominator_tree;

use cfg_ir::function::Function;
use graph::algorithms::back_edges;
use graph::{Edge, Graph, NodeId};

use ast_ir::Block;

struct Lifter<'a> {
    graph: &'a Graph,
    loop_headers: FxHashSet<NodeId>,
    visited: FxHashSet<NodeId>,
    post_dom_tree: &'a Graph,
    back_edges: FxHashSet<Edge>,
    loop_exits: FxHashSet<NodeId>,
}

impl<'a> Lifter<'a> {
    fn new(graph: &'a Graph, post_dom_tree: &'a Graph) -> Self {
        let back_edges = back_edges(graph)
            .unwrap()
            .iter()
            .cloned()
            .collect::<FxHashSet<_>>();
        let loop_headers = back_edges
            .iter()
            .map(|edge| edge.destination())
            .collect::<FxHashSet<_>>();
        Self {
            graph,
            loop_headers,
            visited: FxHashSet::default(),
            post_dom_tree,
            back_edges,
            loop_exits: FxHashSet::default(),
        }
    }

    fn follow_edge(&mut self, source: NodeId, destination: NodeId, body: &mut Block) {
        if self.visited.contains(&destination) {
            if self.back_edges.contains(&Edge::new(source, destination)) {
                body.statements
                    .push(ast_ir::Stat::Continue(ast_ir::Continue { pos: None }));
            } else {
                if self.loop_exits.contains(&destination) {
                    body.statements
                        .push(ast_ir::Stat::Break(ast_ir::Break { pos: None }));
                }
            }
        } else {
            self.lift_block(destination, body);
        }
    }

    fn lift_conditional(
        &mut self,
        header: NodeId,
        true_branch: NodeId,
        false_branch: NodeId,
        body: &mut Block,
    ) {
        // lua 5.1 conditionals always have an exit, except if both branches return
        let exit_statements = self.post_dom_tree.predecessors(header).next().map(|exit| {
            let mut exit_body = Block::new(None);
            if !self.visited.contains(&exit) {
                self.lift_block(exit, &mut exit_body);
            } else {
                // really we should somehow return None here...
            }
            exit_body
        });
        let mut then_block = Block::new(None);
        self.follow_edge(header, true_branch, &mut then_block);
        let else_block = {
            let mut else_block = Block::new(None);
            self.follow_edge(header, false_branch, &mut else_block);
            if !else_block.statements.is_empty() {
                Some(else_block)
            } else {
                None
            }
        };
        body.statements.push(ast_ir::Stat::If(ast_ir::If {
            pos: None,
            condition: ast_ir::Global {
                pos: None,
                name: "__COND__".into(),
            }
            .into(),
            then_block,
            else_block,
        }));
        if let Some(exit_statements) = exit_statements {
            body.statements.extend(exit_statements.statements);
        }
    }

    fn lift_block_internal(&mut self, node: NodeId, body: &mut Block) {
        // TODO: lift instructions
        body.statements.push(ast_ir::Stat::Comment(ast_ir::Comment {
            pos: None,
            comment: node.to_string(),
        }));

        let successors = self.graph.successors(node).collect::<Vec<_>>();
        match successors.len() {
            0 => {}
            1 => self.follow_edge(node, successors[0], body),
            2 => self.lift_conditional(node, successors[0], successors[1], body),
            _ => panic!("unexpected number of successors"),
        }
    }

    fn block_breaks(body: &Block) -> bool {
        let stats = body
            .statements
            .iter()
            .filter(|stat| !matches!(stat, ast_ir::Stat::Comment(_)))
            .collect::<Vec<_>>();
        return stats.len() == 1 && matches!(stats[0], ast_ir::Stat::Break(_));
    }

    fn combine_conditions(first: ast_ir::Expr, second: ast_ir::Expr) -> ast_ir::Expr {
        if let ast_ir::Expr::Lit(lit) = &first {
            if lit.lit == ast_ir::Lit::Boolean(true) {
                return second;
            }
        }
        ast_ir::Binary {
            pos: None,
            op: ast_ir::BinaryOp::LogicalAnd,
            lhs: Box::new(first),
            rhs: Box::new(second),
        }
        .into()
    }

    fn optimize_while(while_stat: &mut ast_ir::While) -> bool {
        let mut stats = while_stat
            .body
            .statements
            .iter_mut()
            .filter(|stat| !matches!(stat, ast_ir::Stat::Comment(_)))
            .collect::<Vec<_>>();
        if stats.len() == 1 {
            if let ast_ir::Stat::If(if_stat) = stats.get_mut(0).unwrap() {
                if let Some(else_block) = &if_stat.else_block {
                    if Self::block_breaks(else_block) {
                        while_stat.condition = Self::combine_conditions(
                            while_stat.condition.clone(),
                            if_stat.condition.clone(),
                        );
                        while_stat.body = if_stat.then_block.clone();
                        return true;
                    } else if Self::block_breaks(&if_stat.then_block) {
                        while_stat.condition = Self::combine_conditions(
                            while_stat.condition.clone(),
                            ast_ir::Unary {
                                pos: None,
                                op: ast_ir::UnaryOp::LogicalNot,
                                expr: Box::new(if_stat.condition.clone()),
                            }
                            .into(),
                        );
                        while_stat.body = else_block.clone();
                        return true;
                    }
                }
            }
        }
        false
    }

    fn lift_block(&mut self, node: NodeId, body: &mut Block) {
        self.visited.insert(node);
        if self.loop_headers.contains(&node) {
            let exit_statements = self.post_dom_tree.predecessors(node).next().map(|exit| {
                let mut exit_body = Block::new(None);
                if !self.visited.contains(&exit) {
                    self.loop_exits.insert(exit);
                    self.lift_block(exit, &mut exit_body);
                } else {
                    // really we should somehow return None here...
                }
                exit_body
            });
            let mut while_body = ast_ir::Block::new(None);
            self.lift_block_internal(node, &mut while_body);
            let mut while_stat = ast_ir::While {
                pos: None,
                condition: ast_ir::ExprLit {
                    pos: None,
                    lit: ast_ir::Lit::Boolean(true),
                }
                .into(),
                body: while_body,
            };
            loop {
                if !Self::optimize_while(&mut while_stat) {
                    break;
                }
            }
            body.statements.push(ast_ir::Stat::While(while_stat));
            if let Some(exit_statements) = exit_statements {
                body.statements.extend(exit_statements.statements);
            }
        } else {
            self.lift_block_internal(node, body);
        }
    }

    fn lift(&mut self, node: NodeId) -> Block {
        let mut root = Block::new(None);
        self.lift_block(node, &mut root);
        root
    }
}

pub fn lift(cfg: &Function) {
    let graph = cfg.graph();
    let entry = graph.entry().unwrap();

    let post_dom_tree = post_dominator_tree(graph, entry).unwrap();
    let mut lifter = Lifter::new(graph, &post_dom_tree);

    let mut ast_function = ast_ir::Function::new();
    ast_function.body = lifter.lift(entry);

    println!("{}", ast_ir::formatter::format_ast(&ast_function));
}
