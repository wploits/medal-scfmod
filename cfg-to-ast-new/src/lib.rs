use cfg::block::BasicBlock;
use fxhash::FxHashMap;
use graph::{
    algorithms::{dominators::*, *},
    *,
};
use itertools::Itertools;

mod conditional;

struct GraphStructurer<'a> {
    graph: Graph,
    root: NodeId,
    idoms: &'a FxHashMap<NodeId, NodeId>,
    blocks: FxHashMap<NodeId, ast::Block<'a>>,
    back_edges: Vec<Edge>,
    loop_exits: Vec<NodeId>,
}

impl<'a> GraphStructurer<'a> {
    fn new(
        graph: Graph,
        blocks: FxHashMap<NodeId, ast::Block<'a>>,
        root: NodeId,
        idoms: &'a FxHashMap<NodeId, NodeId>,
    ) -> Self {
        let back_edges = back_edges(&graph, root).unwrap();
        let post_dom_tree = post_dominator_tree(&graph, &dfs_tree(&graph, root).unwrap()).unwrap();
        let loop_exits = back_edges
            .iter()
            .filter_map(|e| post_dom_tree.predecessors(e.destination).next())
            .collect();
        Self {
            graph,
            blocks,
            root,
            idoms,
            back_edges,
            loop_exits,
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

    fn is_loop_header(&self, node: NodeId) -> bool {
        self.back_edges.iter().any(|edge| edge.destination == node)
    }

    fn try_match_pattern(&mut self, node: NodeId) {
        let successors = self.graph.successors(node).collect::<Vec<_>>();

        for &successor in &successors {
            let successor_successors = self.graph.successors(successor).collect::<Vec<_>>();
            if self.graph.predecessors(successor).count() == 1
                && successor_successors.len() == 1
                && self.blocks[&successor].is_empty()
            {
                self.graph.remove_node(successor).unwrap();
                self.graph
                    .add_edge((node, successor_successors[0]).into())
                    .unwrap();
                dot::render_to(&self.graph, &mut std::io::stdout());
            }
        }

        let successors = self.graph.successors(node).collect::<Vec<_>>();

        if self.is_loop_header(node) {
            let post_dom_tree =
                post_dominator_tree(&self.graph, &dfs_tree(&self.graph, self.root).unwrap())
                    .unwrap();
            let loop_exit = post_dom_tree.predecessors(node).next();
            if let Some(loop_exit) = loop_exit {
                println!("successors: {:?}", successors);
                assert!(successors.contains(&loop_exit));
                assert!(self.graph.predecessors(loop_exit).count() == 1);
                let mut old_block = self.blocks.remove(&node).unwrap();
                if successors.len() == 2 {
                    let loop_body = if successors[0] != loop_exit {
                        successors[0]
                    } else {
                        successors[1]
                    };
                    let if_stat = old_block.last_mut().unwrap().as_if_mut().unwrap();
                    if_stat.then_block = Some(self.blocks.remove(&loop_body).unwrap());
                    if_stat.else_block = Some(ast::Block::from_vec(vec![ast::Break {}.into()]));
                }
                let mut loop_block = ast::Block::from_vec(vec![ast::While::new(
                    ast::Literal::from(true).into(),
                    old_block,
                )
                .into()]);
                loop_block.extend(self.blocks.remove(&loop_exit).unwrap().0);
                self.blocks.insert(node, loop_block);
                //println!("loop body: {}", loop_body);
            } else {
                panic!("no single exit for loop");
            }
        } else {
            if successors.len() == 2 {
                self.match_conditional(node, successors[0], successors[1]);
                println!("matched conditional");
            } else if successors.len() == 1 {
                println!("node: {}", node);
                if let Some(block) = self.match_virtual_branch(node, successors[0]) {
                    println!("matched virtual branch");
                    self.blocks.get_mut(&node).unwrap().extend(block.0);
                }
            }
        }

        /*if self.is_loop_header(node) {
            let mut block = self.blocks.remove(&node).unwrap();
            if let Some(last) = block.last() {
                if matches!(last, ast::Statement::Continue(_) | ast::Statement::Break(_)) {
                    block.pop();
                }
            }
            self.blocks.insert(
                node,
                ast::Block::from_vec(vec![ast::While::new(
                    ast::Literal::from(true).into(),
                    block,
                )
                .into()]),
            );
        }*/
    }

    fn match_virtual_branch(&mut self, node: NodeId, branch: NodeId) -> Option<ast::Block<'a>> {
        if self.loop_header(node) == Some(branch) {
            println!("its a continue");
            let mut block = ast::Block::new();
            block.push(ast::Continue {}.into());
            //self.graph.remove_edge((node, branch).into()).unwrap();
            Some(block)
        } else if self.loop_exits.contains(&branch) {
            println!("its a break");
            let mut block = ast::Block::new();
            block.push(ast::Break {}.into());
            //self.graph.remove_edge((node, branch).into()).unwrap();
            Some(block)
        } else {
            None
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
