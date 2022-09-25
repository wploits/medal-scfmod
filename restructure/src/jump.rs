use itertools::Itertools;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

impl super::GraphStructurer {
    pub(crate) fn match_jump(
        &mut self,
        node: NodeIndex,
        target: Option<NodeIndex>,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        if let Some(target) = target {
            if node == target {
                return false;
            }
            if Self::block_is_no_op(&self.function.block(node).unwrap().ast) {
                for predecessor in self.function.predecessor_blocks(node).collect_vec() {
                    self.function.replace_edge(predecessor, node, target);
                }
                self.function.remove_block(node);
                true
            } else if self.function.predecessor_blocks(target).count() == 1
                && dominators.dominators(target).unwrap().contains(&node)
            {
                let block = self.function.remove_block(target).unwrap();
                let terminator = block.terminator;
                self.function
                    .block_mut(node)
                    .unwrap()
                    .ast
                    .extend(block.ast.0);
                self.function.set_block_terminator(node, terminator);
                true
            } else {
                false
            }
        } else if Self::block_is_no_op(&self.function.block(node).unwrap().ast) {
            let preds = self.function.predecessor_blocks(node).collect_vec();
            let mut invalid = false;
            for &pred in &preds {
                if self.function.successor_blocks(pred).collect_vec().len() != 1 {
                    invalid = true;
                    break;
                }
            }
            if !invalid {
                for pred in preds {
                    self.function.remove_edge(pred, node);
                    self.function.set_block_terminator(pred, None);
                }
                self.function.remove_block(node);
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}
