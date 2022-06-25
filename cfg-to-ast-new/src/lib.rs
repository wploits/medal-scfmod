use std::{iter::successors, process::exit};

use fxhash::FxHashMap;
use graph::{
    algorithms::{dominators::*, *},
    *,
};
use itertools::Itertools;

struct GraphStructurer<'a> {
    graph: Graph,
    root: NodeId,
    idoms: &'a FxHashMap<NodeId, NodeId>,
    blocks: FxHashMap<NodeId, ast::Block<'a>>,
    back_edges: Vec<Edge>,
}

impl<'a> GraphStructurer<'a> {
    fn new(
        graph: Graph,
        blocks: FxHashMap<NodeId, ast::Block<'a>>,
        root: NodeId,
        idoms: &'a FxHashMap<NodeId, NodeId>,
    ) -> Self {
        let back_edges = back_edges(&graph, root).unwrap();
        Self {
            graph,
            blocks,
            root,
            idoms,
            back_edges,
        }
    }

    fn loop_header(&self, mut node: NodeId) -> Option<NodeId> {
        while !self.back_edges.iter().any(|edge| edge.destination == node) {
            if let Some(&idom) = self.idoms.get(&node) {
                node = idom;
            } else {
                return None;
            }
        }
        Some(node)
    }

    fn match_infinite_loop(&mut self, header: NodeId, latches: &[NodeId]) -> bool {
        let successors = self.graph.successors(header).collect::<Vec<_>>();
        if latches.len() == 1
            && successors.len() == 1
            && self.graph.successors(latches[0]).count() == 1
            && successors[0] == header
        {
            self.graph.remove_edge((latches[0], header).into()).unwrap();
            let block = self.blocks.remove(&header).unwrap();
            self.blocks.insert(
                header,
                ast::Block::from_vec(vec![ast::While::new(
                    ast::Literal::from(true).into(),
                    block,
                )
                .into()]),
            );
            return true;
        }
        false
    }

    fn is_loop_header(&self, node: NodeId) -> bool {
        self.back_edges.iter().any(|edge| edge.destination == node)
    }

    fn match_empty_jump(&mut self, node: NodeId) -> bool {
        let successors = self.graph.successors(node).collect::<Vec<_>>();
        if successors.len() == 1 {
            let target = successors[0];
            let target_successors = self.graph.successors(target).collect::<Vec<_>>();
            if self.graph.predecessors(target).count() == 1 && target_successors.len() < 2 {
                if !target_successors.is_empty() {
                    self.graph.add_edge((node, target).into()).unwrap();
                }
                self.graph.remove_node(target).unwrap();
                let target_block = self.blocks.remove(&target).unwrap();
                self.blocks.get_mut(&node).unwrap().extend(target_block.0);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn try_match_pattern(&mut self, node: NodeId) {
        let successors = self.graph.successors(node).collect::<Vec<_>>();
        if successors.len() == 2 {
            self.match_conditional(node, successors[1], successors[0]);
            println!("matched conditional");
            if self.is_loop_header(node) {
                let block = self.blocks.get_mut(&node).unwrap();
                assert!(block.len() == 1);
                let mut if_stat = match block.pop().unwrap() {
                    ast::Statement::If(if_stat) => if_stat,
                    _ => unreachable!(),
                };
                let exit_statements = if_stat.else_block.unwrap();
                if_stat.else_block = Some(ast::Block::from_vec(vec![ast::Break {}.into()]));
                let while_stat = ast::While::new(
                    ast::Literal::from(true).into(),
                    ast::Block::from_vec(vec![if_stat.into()]),
                );
                block.push(while_stat.into());
                block.extend(exit_statements.0);
            }
        } else if successors.len() == 1 {
            if self.match_empty_jump(node) {
                println!("unnecessary jump removed");
            } else if let Some(block) = self.match_virtual_branch(node, successors[0]) {
                println!("matched virtual branch");
                self.blocks.get_mut(&node).unwrap().extend(block.0);
            }
            if self.is_loop_header(node) {
                println!("matched inf loop");
                assert!(successors[0] == node);
                let block = self.blocks.remove(&node).unwrap();
                self.blocks.insert(
                    node,
                    ast::Block::from_vec(vec![ast::While::new(
                        ast::Literal::from(true).into(),
                        block,
                    )
                    .into()]),
                );
            }
        }
    }

    fn match_virtual_branch(&mut self, node: NodeId, branch: NodeId) -> Option<ast::Block<'a>> {
        if self.back_edges.contains(&Edge::new(node, branch)) {
            let mut block = ast::Block::new();
            block.push(ast::Continue {}.into());
            self.graph.remove_edge((node, branch).into()).unwrap();
            Some(block)
        } else {
            None
        }
    }

    fn match_conditional(&mut self, entry: NodeId, then_node: NodeId, else_node: NodeId) {
        let mut exit_block = None;
        let mut then_block = self.match_virtual_branch(entry, then_node);
        let mut else_block = self.match_virtual_branch(entry, else_node);

        let then_successors = self.graph.successors(then_node).collect::<Vec<_>>();
        let then_predecessors = self.graph.predecessors(then_node).collect::<Vec<_>>();
        let else_successors = self.graph.successors(else_node).collect::<Vec<_>>();
        let else_predecessors = self.graph.predecessors(else_node).collect::<Vec<_>>();

        if then_block.is_none() && else_block.is_none() {
            // conditional?
            // todo: check dfs tree instead?
            if ((then_successors.len() == 1
                && else_successors.len() == 1
                && then_successors[0] == else_successors[0])
                || then_successors.is_empty() && else_successors.is_empty())
                && then_predecessors.len() == 1
                && else_predecessors.len() == 1
            {
                // if then else end
                dot::render_to(&self.graph, &mut std::io::stdout());
                then_block = Some(self.blocks.remove(&then_node).unwrap());
                else_block = Some(self.blocks.remove(&else_node).unwrap());
                self.graph.remove_node(then_node).unwrap();
                self.graph.remove_node(else_node).unwrap();
                if !else_successors.is_empty() {
                    exit_block = Some(self.blocks.remove(&else_successors[0]).unwrap());
                    self.graph.remove_node(else_successors[0]).unwrap();
                }
            }
            // single branch conditional?
            else if then_successors.len() == 1
                && else_node == then_successors[0]
                && self.graph.predecessors(else_node).count() == 2
                && self.graph.predecessors(then_node).count() == 1
            {
                then_block = Some(self.blocks.remove(&then_node).unwrap());
                exit_block = Some(self.blocks.remove(&else_node).unwrap());
                self.graph.remove_node(then_node).unwrap();
                self.graph.remove_node(else_node).unwrap();
            } else if else_successors.len() == 1
                && then_node == else_successors[0]
                && self.graph.predecessors(then_node).count() == 2
                && self.graph.predecessors(else_node).count() == 1
            {
                else_block = Some(self.blocks.remove(&else_node).unwrap());
                exit_block = Some(self.blocks.remove(&then_node).unwrap());
                self.graph.remove_node(else_node).unwrap();
                self.graph.remove_node(then_node).unwrap();
            } else {
                panic!("no pattern matched");
            }
        } else if then_block.is_some() != else_block.is_some() {
            // one branch has a virtual edge
            if else_block.is_some() {
                std::mem::swap(&mut then_block, &mut else_block);
            }
            println!("{}", else_node);
            exit_block = Some(self.blocks.remove(&else_node).unwrap());
            else_block = None;
        } else {
            panic!("no pattern matched");
        }
        println!("{}", entry);
        let block = self.blocks.get_mut(&entry).unwrap();
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();

        if then_block.is_none() && else_block.is_some() {
            std::mem::swap(&mut then_block, &mut else_block);
            if_stat.condition = Box::new(
                ast::Unary::new(*if_stat.condition.clone(), ast::UnaryOperation::Not).into(),
            );
        }

        if let Some(then_block) = then_block {
            if_stat.then_block = Some(then_block);
        }
        if let Some(else_block) = else_block {
            if_stat.else_block = Some(else_block);
        }
        if let Some(exit_block) = exit_block {
            block.extend(exit_block.0);
        }
    }

    fn collapse(&mut self) {
        let dfs = dfs_tree(&self.graph, self.root).unwrap();
        for node in self
            .graph
            .nodes()
            .iter()
            .filter(|&&node| !dfs.node_exists(node))
            .cloned()
            .collect::<Vec<_>>()
        {
            self.graph.remove_node(node).unwrap();
        }

        for node in dfs.post_order(self.root).unwrap() {
            println!("matching {}", node);
            self.try_match_pattern(node);
            dot::render_to(&self.graph, &mut std::io::stdout());
        }
        let nodes = self.graph.nodes().len();
        if self.graph.nodes().len() != 1 {
            println!("failed to collapse! total nodes: {}", nodes);
        }
    }

    fn structure(mut self) -> ast::Block<'a> {
        self.collapse();
        self.blocks.remove(&self.root).unwrap()
    }
}

pub fn lift(function: cfg::function::Function) {
    let graph = function.graph().clone();
    let root = function.entry().unwrap();
    let dfs = dfs_tree(&graph, root).unwrap();
    let idoms = compute_immediate_dominators(&graph, root, &dfs).unwrap();

    dot::render_to(&graph, &mut std::io::stdout());

    let blocks = function
        .blocks()
        .iter()
        .map(|(&node, block)| (node, block.block.clone()))
        .collect();

    let structurer = GraphStructurer::new(graph, blocks, root, &idoms);
    let block = structurer.structure();
    println!("{}", block);
}
