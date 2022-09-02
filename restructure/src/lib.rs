use cfg::{block::BasicBlock, dot, function::Function, inline::inline_expressions};
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    stable_graph::NodeIndex,
    visit::*,
};

mod compound;
mod conditional;
mod jump;
mod r#loop;

struct GraphStructurer {
    pub function: Function,
    root: NodeIndex,
    loop_headers: FxHashSet<NodeIndex>,
}

impl GraphStructurer {
    fn new(function: Function, graph: StableDiGraph<BasicBlock, ()>, root: NodeIndex) -> Self {
        let root = function.entry().unwrap();
        let mut loop_headers = FxHashSet::default();
        depth_first_search(&graph, Some(root), |event| {
            if let DfsEvent::BackEdge(_, header) = event {
                loop_headers.insert(header);
            }
        });

        Self {
            function,
            root,
            loop_headers,
        }
    }

    fn block_is_no_op(block: &ast::Block) -> bool {
        block
            .iter()
            .filter(|stmt| stmt.as_comment().is_some())
            .count()
            == block.len()
    }

    fn try_match_pattern(&mut self, node: NodeIndex, dominators: &Dominators<NodeIndex>) -> bool {
        let successors = self.function.successor_blocks(node).collect_vec();

        println!("before");
        cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

        if self.try_collapse_loop(node, dominators) {
            println!("changed");
            cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();
            return true;
        }

        let changed = match successors.len() {
            0 => false,
            1 => {
                // remove unnecessary jumps to allow pattern matching
                self.match_jump(node, successors[0], dominators)
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
                self.match_compound_conditional(node, then_node, else_node)
                    || self.match_conditional(node, then_node, else_node, dominators)
            }

            _ => unreachable!(),
        };

        println!("after");
        dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

        changed
    }

    fn match_blocks(&mut self) -> bool {
        let dfs = Dfs::new(self.function.graph(), self.root)
            .iter(self.function.graph())
            .collect::<FxHashSet<_>>();
        let mut dfs_postorder = DfsPostOrder::new(self.function.graph(), self.root);
        let dominators = simple_fast(self.function.graph(), self.function.entry().unwrap());

        for node in self
            .function
            .graph()
            .node_indices()
            .filter(|node| !dfs.contains(node))
            .collect_vec()
        {
            self.function.remove_block(node);
        }

        let mut changed = false;
        while let Some(node) = dfs_postorder.next(self.function.graph()) {
            println!("matching {:?}", node);
            changed |= self.try_match_pattern(node, &dominators);
        }

        cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

        changed
    }

    fn collapse(&mut self) {
        while self.match_blocks() {}

        let nodes = self.function.graph().node_count();
        if self.function.graph().node_count() != 1 {
            println!("failed to collapse! total nodes: {}", nodes);
        }
    }

    fn structure(mut self) -> ast::Block {
        self.collapse();
        self.function.remove_block(self.root).unwrap().ast
    }
}

pub fn lift(function: cfg::function::Function) -> ast::Block {
<<<<<<< HEAD
    let graph = function.graph().clone();
    let root = function.entry().unwrap();

    let structurer = GraphStructurer::new(function, graph, root);
=======
    //dot::render_to(&graph, &mut std::io::stdout());

    let structurer = GraphStructurer::new(function);
>>>>>>> 1e49a6df213ccb1c6e0ccfd587c47c43a8cdb332
    structurer.structure()
}
