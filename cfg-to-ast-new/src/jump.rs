use graph::NodeId;

impl<'a> super::GraphStructurer<'a> {
    pub(crate) fn match_jump(&mut self, node: NodeId, target: NodeId) -> bool {
        if Self::block_is_no_op(&self.function.block(node).unwrap().ast) {
            for predecessor in self.function.graph().predecessors(node) {
                self.function.replace_edge(&(predecessor, node), target);
            }
            self.function.remove_block(node);
            true
        } else if self.function.graph().predecessors(target).len() == 1 {
            let terminator = self.function.block(target).unwrap().terminator.clone();
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