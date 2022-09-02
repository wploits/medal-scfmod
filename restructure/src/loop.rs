use ast::Reduce;
use cfg::block::Terminator;
use itertools::Itertools;
use std::collections::HashMap;

use crate::GraphStructurer;
use petgraph::{
    algo::dominators::{simple_fast, Dominators},
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

    pub(crate) fn try_collapse_loop(
        &mut self,
        header: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
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
        } else if successors.len() == 2 {
            // cant turn into a while loop if there are more statements in the block
            if self.function.block(header).unwrap().ast.len() > 1 {
                return false;
            }

            let (body, next) = if let [node_1, node_2] = successors[..] {
                if self.function.successor_blocks(node_1).contains(&header) {
                    assert!(!self.function.successor_blocks(node_2).contains(&header));
                    (node_1, node_2)
                } else {
                    (node_2, node_1)
                }
            } else {
                panic!();
            };

            assert!(self.function.successor_blocks(body).count() == 1);
            assert!(self.function.successor_blocks(next).count() <= 1);
            assert!(self.function.predecessor_blocks(next).count() == 1);

            let mut if_condition = *self
                .function
                .block_mut(header)
                .unwrap()
                .ast
                .remove(0)
                .into_if()
                .unwrap()
                .condition;
            if self
                .function
                .block(header)
                .unwrap()
                .terminator
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap()
                .1
                .node
                == body
            {
                if_condition = ast::Unary::new(if_condition, ast::UnaryOperation::Not).reduce();
            }

            let while_stat =
                ast::While::new(if_condition, self.function.remove_block(body).unwrap().ast);
            self.function
                .block_mut(header)
                .unwrap()
                .ast
                .push(while_stat.into());
            self.function
                .set_block_terminator(header, Some(Terminator::jump(next)));
            self.match_jump(header, next, dominators);
            true
        } else {
            false
        }
    }
}
