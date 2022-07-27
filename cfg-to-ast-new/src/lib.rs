use cfg::dot;
use cfg::function::Function;
use fxhash::FxHashMap;
use graph::{
    algorithms::{dominators::*, *},
    Edge, Graph, NodeId,
};

mod compound;
mod conditional;
mod jump;
mod r#loop;

struct GraphStructurer<'a> {
    function: Function<'a>,
    root: NodeId,
    back_edges: Vec<Edge>,
}

impl<'a> GraphStructurer<'a> {
    fn new(
        function: Function<'a>,
        graph: Graph,
        blocks: FxHashMap<NodeId, ast::Block<'a>>,
        root: NodeId,
        _idoms: &'a FxHashMap<NodeId, NodeId>,
    ) -> Self {
        let back_edges = back_edges(&graph, root).unwrap();
        let root = function.entry().unwrap();
        Self {
            function,
            root,
            back_edges,
        }
    }

    fn block_is_no_op(block: &ast::Block) -> bool {
        block
            .iter()
            .filter(|stmt| stmt.as_comment().is_some())
            .count()
            == block.len()
    }

    fn try_match_pattern(&mut self, node: NodeId) {
        let successors = self.function.graph().successors(node);

        if self.try_collapse_loop(node) {
            return;
        }

        match successors.len() {
            0 => {}
            1 => {
                // remove unnecessary jumps to allow pattern matching
                self.match_jump(node, successors[0]);
            }
            2 => {
                let (then_edge, else_edge) = self
                    .function
                    .block(node)
                    .unwrap()
                    .terminator
                    .as_ref()
                    .unwrap()
                    .as_conditional()
                    .unwrap();
                let (then_node, else_node) = (then_edge.node, else_edge.node);
                //self.match_conditional(node, then_node, else_node);
                if self.match_compound_conditional(node, then_node, else_node) {
                    self.try_match_pattern(node);
                }
            }
            _ => unreachable!(),
        };
        dot::render_to(&self.function, &mut std::io::stdout());
    }

    fn collapse(&mut self) {
        let dfs = dfs_tree(self.function.graph(), self.root);
        for node in self
            .function
            .graph()
            .nodes()
            .iter()
            .filter(|&&node| !dfs.has_node(node))
            .cloned()
            .collect::<Vec<_>>()
        {
            self.function.remove_block(node);
        }

        for node in dfs.post_order(self.root) {
            println!("matching {}", node);
            self.try_match_pattern(node);
        }

        let nodes = self.function.graph().nodes().len();
        if self.function.graph().nodes().len() != 1 {
            println!("failed to collapse! total nodes: {}", nodes);
        }
    }

    fn structure(mut self) -> ast::Block<'a> {
        self.collapse();
        self.function.remove_block(self.root).unwrap().ast
    }
}

pub fn lift(function: cfg::function::Function) {
    let graph = function.graph().clone();
    let root = function.entry().unwrap();
    let dfs = dfs_tree(&graph, root);
    let idoms = compute_immediate_dominators(&graph, root, &dfs);

    //dot::render_to(&graph, &mut std::io::stdout());

    let blocks = function
        .blocks()
        .iter()
        .map(|(&node, block)| (node, block.ast.clone()))
        .collect();

    let structurer = GraphStructurer::new(function, graph, blocks, root, &idoms);
    let block = structurer.structure();
    println!("{}", block);
}
