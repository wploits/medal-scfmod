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
        }
    }

    fn follow_edge(&mut self, source: NodeId, destination: NodeId, body: &mut Block) {
        if self.visited.contains(&destination) {
            println!("{} -> {}, destination already visited", source, destination);
            if self.back_edges.contains(&Edge::new(source, destination)) {
                body.statements
                    .push(ast_ir::Stat::Continue(ast_ir::Continue { pos: None }));
            } else {
                // TODO: extra checks required?
                body.statements
                    .push(ast_ir::Stat::Break(ast_ir::Break { pos: None }));
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
            condition: ast_ir::ExprLit {
                pos: None,
                lit: ast_ir::Lit::Boolean(true),
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

    fn lift_block(&mut self, node: NodeId, body: &mut Block) {
        self.visited.insert(node);
        if self.loop_headers.contains(&node) {
            let exit_statements = self.post_dom_tree.predecessors(node).next().map(|exit| {
                let mut exit_body = Block::new(None);
                if !self.visited.contains(&exit) {
                    println!("loop exit: {}", exit);
                    self.lift_block(exit, &mut exit_body);
                } else {
                    // really we should somehow return None here...
                }
                exit_body
            });
            let mut while_body = ast_ir::Block::new(None);
            self.lift_block_internal(node, &mut while_body);
            let while_stat = ast_ir::While {
                pos: None,
                cond: ast_ir::ExprLit {
                    pos: None,
                    lit: ast_ir::Lit::Boolean(true),
                }
                .into(),
                body: while_body,
            };
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
