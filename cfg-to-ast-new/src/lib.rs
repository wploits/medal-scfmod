use fxhash::FxHashMap;
use graph::{
    algorithms::{dominators::*, *},
    *,
};

mod conditional;
mod optimizer;

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

    fn is_loop_header(&self, node: NodeId) -> bool {
        self.back_edges.iter().any(|edge| edge.destination == node)
    }

    fn match_empty_jump(&mut self, node: NodeId) -> bool {
        let successors = self.graph.successors(node).collect::<Vec<_>>();
        if successors.len() == 1 {
            let target = successors[0];
            if target != node {
                let target_successors = self.graph.successors(target).collect::<Vec<_>>();
                if self.graph.predecessors(target).count() == 1 && target_successors.len() < 2 {
                    if !target_successors.is_empty() {
                        self.graph.add_edge((node, target).into()).unwrap();
                    }
                    self.graph.remove_node(target).unwrap();
                    let target_block = self.blocks.remove(&target).unwrap();
                    self.blocks.get_mut(&node).unwrap().extend(target_block.0);
                    return true;
                }
            }
        }
        false
    }

    fn try_match_pattern(&mut self, node: NodeId) {
        let successors = self.graph.successors(node).collect::<Vec<_>>();
        if successors.len() == 2 {
            self.match_conditional(node, successors[1], successors[0]);
            println!("matched conditional");
        } else if successors.len() == 1 {
            if self.match_empty_jump(node) {
                println!("unnecessary jump removed");
            } else if let Some(block) = self.match_virtual_branch(node, successors[0]) {
                println!("matched virtual branch");
                self.blocks.get_mut(&node).unwrap().extend(block.0);
            }
        }
        if self.is_loop_header(node) {
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
        }
    }

    fn match_virtual_branch(&mut self, node: NodeId, branch: NodeId) -> Option<ast::Block<'a>> {
        if self.back_edges.contains(&Edge::new(node, branch)) {
            let mut block = ast::Block::new();
            if branch == self.loop_header(node).unwrap() {
                println!("its a continue");
                block.push(ast::Continue {}.into());
            }
            self.graph.remove_edge((node, branch).into()).unwrap();
            Some(block)
        } else {
            let mut block = ast::Block::new();
            block.push(ast::Break {}.into());
            println!("other edge, might be break");
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
