#![feature(let_chains)]

use std::iter;

use cfg::function::Function;
use fxhash::FxHashSet;
use itertools::Itertools;

use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    stable_graph::{NodeIndex, StableDiGraph},
    visit::*,
};

mod conditional;
mod jump;
mod r#loop;

pub fn post_dominators<N: Default, E: Default>(
    graph: &mut StableDiGraph<N, E>,
) -> Dominators<NodeIndex> {
    let exits = graph
        .node_identifiers()
        .filter(|&n| graph.neighbors(n).count() == 0)
        .collect_vec();
    let fake_exit = graph.add_node(Default::default());
    for exit in exits {
        graph.add_edge(exit, fake_exit, Default::default());
    }
    let res = simple_fast(Reversed(&*graph), fake_exit);
    assert!(graph.remove_node(fake_exit).is_some());
    res
}

struct GraphStructurer {
    pub function: Function,
    root: NodeIndex,
    loop_headers: FxHashSet<NodeIndex>,
}

impl GraphStructurer {
    fn new(function: Function) -> Self {
        let root = function.entry().unwrap();
        let mut loop_headers = FxHashSet::default();
        depth_first_search(function.graph(), Some(root), |event| {
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
        !block.iter().any(|s| s.as_comment().is_none())
    }

    fn try_match_pattern(&mut self, node: NodeIndex, dominators: &Dominators<NodeIndex>) -> bool {
        let successors = self.function.successor_blocks(node).collect_vec();

        //println!("before");
        //cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

        if self.try_collapse_loop(node, dominators) {
            // println!("matched loop");
            return true;
        }

        let changed = match successors.len() {
            0 => false,
            1 => {
                // remove unnecessary jumps to allow pattern matching
                self.match_jump(node, Some(successors[0]), dominators)
            }
            2 => {
                let (then_edge, else_edge) = self.function.conditional_edges(node).unwrap();
                self.match_conditional(node, then_edge.target(), else_edge.target(), dominators)
            }

            _ => unreachable!(),
        };

        //println!("after");
        //dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

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

        //cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();

        let mut changed = false;
        while let Some(node) = dfs_postorder.next(self.function.graph()) {
            // println!("matching {:?}", node);
            let matched = self.try_match_pattern(node, &dominators);
            changed |= matched;
            // if matched {
            //     cfg::dot::render_to(&self.function, &mut std::io::stdout()).unwrap();
            // }
        }

        changed
    }

    fn collapse(&mut self) {
        while self.match_blocks() {}
    }

    fn structure(mut self) -> ast::Block {
        self.collapse();
        let nodes = self.function.graph().node_count();
        if self.function.graph().node_count() != 1 {
            iter::once(
                ast::Comment::new(format!("failed to collapse, total nodes: {}", nodes)).into(),
            )
            .chain(self.function.remove_block(self.root).unwrap().0.into_iter())
            .collect::<Vec<_>>()
            .into()
        } else {
            self.function.remove_block(self.root).unwrap()
        }
    }
}

pub fn lift(function: cfg::function::Function) -> ast::Block {
    GraphStructurer::new(function).structure()
}
