use ast::Reduce;
use cfg::block::Terminator;
use itertools::Itertools;

use crate::GraphStructurer;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

impl GraphStructurer {
    fn simplify_if(if_stat: &mut ast::If) {
        if let Some(unary) = if_stat.condition.as_unary() {
            if unary.operation == ast::UnaryOperation::Not {
                if_stat.condition = *unary.value.clone();
                std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
            }
        }
    }

    // a -> b -> d + a -> c -> d
    fn match_diamond_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        let then_successors = self.function.successor_blocks(then_node).collect_vec();
        let else_successors = self.function.successor_blocks(else_node).collect_vec();

        if then_successors.len() != 1 || then_successors != else_successors {
            return false;
        }

        if self.function.predecessor_blocks(then_node).count() != 1
            || self.function.predecessor_blocks(else_node).count() != 1
        {
            return false;
        }

        let then_block = self.function.remove_block(then_node).unwrap();
        let else_block = self.function.remove_block(else_node).unwrap();

        let block = self.function.block_mut(entry).unwrap();
        let if_stat = block.ast.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = Some(then_block.ast);
        if_stat.else_block = Some(else_block.ast);
        Self::simplify_if(if_stat);

        let exit = then_successors[0];
        self.function
            .set_block_terminator(entry, Some(Terminator::jump(exit)));
        self.match_jump(entry, exit, dominators);

        true
    }

    // a -> b -> c + a -> c
    fn match_triangle_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        let mut _match_triangle_conditional = |then_node, else_node, inverted| {
            let then_successors = self.function.successor_blocks(then_node).collect_vec();

            if then_successors.len() != 1 {
                return false;
            }

            if self.function.predecessor_blocks(then_node).count() != 1 {
                return false;
            }

            if then_successors[0] != else_node {
                return false;
            }

            let then_block = self.function.remove_block(then_node).unwrap();

            let block = self.function.block_mut(entry).unwrap();
            let if_stat = block.ast.last_mut().unwrap().as_if_mut().unwrap();
            if_stat.then_block = Some(then_block.ast);

            if inverted {
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce()
            }

            //Self::simplify_if(if_stat);

            self.function
                .set_block_terminator(entry, Some(Terminator::jump(else_node)));
            self.match_jump(entry, else_node, dominators);

            true
        };

        _match_triangle_conditional(then_node, else_node, false)
            || _match_triangle_conditional(else_node, then_node, true)
    }

    pub(crate) fn refine_edge(
        &mut self,
        entry: NodeIndex,
        back_edge: NodeIndex,
        next: NodeIndex,
        dominators: &Dominators<NodeIndex>,
        invert_condition: bool,
        statement: ast::Statement,
    ) -> bool {
        let block = self.function.block_mut(entry).unwrap();
        let if_stat = block.ast.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = Some(ast::Block::from_vec(vec![statement]));
        if invert_condition {
            if_stat.condition =
                ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce();
        }
        self.function
            .set_block_terminator(entry, Some(Terminator::jump(next)));
        self.match_jump(entry, next, dominators);
        true
    }

    pub(crate) fn match_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
        loop_refinement: bool,
    ) -> bool {
        self.match_diamond_conditional(entry, then_node, else_node, dominators)
            || self.match_triangle_conditional(entry, then_node, else_node, dominators)
    }
}
