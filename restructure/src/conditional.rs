use ast::Reduce;
use cfg::block::Terminator;
use itertools::Itertools;

use crate::{post_dominators, GraphStructurer};
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
    // results in a -> d
    fn match_diamond_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        let then_successors = self.function.successor_blocks(then_node).collect_vec();
        let else_successors = self.function.successor_blocks(else_node).collect_vec();

        if then_successors.len() > 1 || then_successors != else_successors {
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

        let exit = then_successors.get(0).cloned();
        if let Some(exit) = exit {
            self.function
                .set_block_terminator(entry, Some(Terminator::jump(exit)));
        } else {
            self.function.set_block_terminator(entry, None);
        }
        self.match_jump(entry, exit, dominators);

        true
    }

    // a -> b -> c + a -> c
    // results in a -> c
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
            self.match_jump(entry, Some(else_node), dominators);

            true
        };

        _match_triangle_conditional(then_node, else_node, false)
            || _match_triangle_conditional(else_node, then_node, true)
    }

    fn match_early_exit_triangle_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        let mut _match_early_exit_triangle_conditional = |then_node, else_node, inverted| {
            let then_successors = self.function.successor_blocks(then_node).collect_vec();

            if !then_successors.is_empty() {
                return false;
            }

            if self.function.predecessor_blocks(then_node).count() != 1 {
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

            self.function
                .set_block_terminator(entry, Some(Terminator::jump(else_node)));
            self.match_jump(entry, Some(else_node), dominators);

            true
        };

        _match_early_exit_triangle_conditional(then_node, else_node, false)
            || _match_early_exit_triangle_conditional(else_node, then_node, true)
    }

    // a -> b a -> c
    pub(crate) fn refine_virtual_edge_jump(
        &mut self,
        _entry: NodeIndex,
        _node: NodeIndex,
        _header: NodeIndex,
        _next: NodeIndex,
    ) -> bool {
        /* let block = &mut self.function.block_mut(entry).unwrap().ast;
        if node == header {
            block.push(ast::Continue {}.into());
        } else if node == next {
            block.push(ast::Break {}.into());
        }
        self.function.set_block_terminator(entry, None); */
        false
    }

    pub(crate) fn refine_virtual_edge_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        header: NodeIndex,
        next: NodeIndex,
        _dominators: &Dominators<NodeIndex>,
    ) -> bool {
        let header_successors = self.function.successor_blocks(header).collect_vec();
        let block = self.function.block_mut(entry).unwrap();
        let if_stat = block.ast.last_mut().unwrap().as_if_mut().unwrap();

        if then_node == header && !header_successors.contains(&entry) {
            if_stat.then_block = Some(vec![ast::Continue {}.into()].into());
        } else if then_node == next {
            if_stat.then_block = Some(vec![ast::Break {}.into()].into());
        }
        if else_node == header && !header_successors.contains(&entry) {
            if_stat.else_block = Some(vec![ast::Continue {}.into()].into());
        } else if else_node == next {
            if_stat.else_block = Some(vec![ast::Break {}.into()].into());
        }
        if if_stat.then_block.is_some() && if_stat.else_block.is_none() {
            self.function
                .set_block_terminator(entry, Some(Terminator::jump(else_node)));
        } else if if_stat.then_block.is_none() && if_stat.else_block.is_some() {
            if_stat.condition =
                ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce();
            std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
            self.function
                .set_block_terminator(entry, Some(Terminator::jump(then_node)));
        } else if if_stat.then_block.is_some() && if_stat.else_block.is_some() {
            self.function.set_block_terminator(entry, None);
        }
        true
    }

    pub(crate) fn match_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        if self.is_loop_header(entry) {
            return false;
        }

        let block = self.function.block_mut(entry).unwrap();
        if block.ast.last_mut().unwrap().as_if_mut().is_none() {
            // for loops
            return false;
        }

        self.match_diamond_conditional(entry, then_node, else_node, dominators)
            || self.match_triangle_conditional(entry, then_node, else_node, dominators)
            || self.match_early_exit_triangle_conditional(entry, then_node, else_node, dominators)
    }
}
