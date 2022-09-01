use itertools::Itertools;
use std::collections::HashMap;

use crate::GraphStructurer;
use petgraph::{
    algo::dominators::simple_fast,
    stable_graph::NodeIndex,
    visit::{Reversed, Visitable},
};

impl GraphStructurer {
    pub(crate) fn is_loop_header(&self, node: NodeIndex) -> bool {
        self.back_edges
            .iter()
            .any(|edge| edge.1 == node)
    }

    fn refine_breaks(
        &mut self,
        header: NodeIndex,
        exit: NodeIndex,
        exit_predecessors: Vec<NodeIndex>,
    ) {
        for predecessor in exit_predecessors {
            if predecessor != header {
                self.function.remove_edge(predecessor, exit);
            }
        }
    }

    pub(crate) fn try_collapse_loop(&mut self, header: NodeIndex) -> bool {
        let successors = self.function.successor_blocks(header).collect::<Vec<_>>();
        if successors.len() == 1 && successors[0] == header {
            let mut blocks: HashMap<_, _> = self.function.blocks_mut();
            let while_stat = ast::While::new(
                ast::Literal::Boolean(true).into(),
                blocks[&header].ast.clone(),
            );
            *blocks.get_mut(&header).unwrap().ast = vec![while_stat.into()];
            self.function.remove_edge(header, header);
            true
        } else {
            false
        }
    }
}
