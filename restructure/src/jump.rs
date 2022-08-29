use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;

impl super::GraphStructurer {
    pub(crate) fn match_jump(&mut self, node: NodeIndex, target: NodeIndex) -> bool {
        if Self::block_is_no_op(&self.function.block(node).unwrap().ast) {
            for predecessor in self.function.predecessor_blocks(node).collect_vec() {
                self.function.replace_edge(predecessor, node, target);
            }
            self.function.remove_block(node);
            true
        } else if self.function.predecessor_blocks(target).count() == 1 {
            let terminator = self.function.block(target).unwrap().terminator.clone();
            println!("terminator: {:?}", terminator);
            let block = self.function.remove_block(target).unwrap();
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
    }
}
