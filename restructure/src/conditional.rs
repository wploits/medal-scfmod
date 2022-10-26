use ast::Reduce;
use cfg::block::{BlockEdge, BranchType};
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

    fn expand_if(if_stat: &mut ast::If) -> Option<ast::Block> {
        if let Some(then_block) = &if_stat.then_block {
            if let Some(last) = then_block.last() && last.as_return().is_some() {
                return if_stat.else_block.take();
            }
        }
        None
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
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = Some(then_block);
        if_stat.else_block = Some(else_block);
        Self::simplify_if(if_stat);

        if let Some(after) = Self::expand_if(if_stat) {
            block.extend(after.0);
        }

        let exit = then_successors.get(0).cloned();
        if let Some(exit) = exit {
            self.function.set_edges(
                entry,
                vec![(exit, BlockEdge::new(BranchType::Unconditional))],
            );
        } else {
            self.function.remove_edges(entry);
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
            let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
            if_stat.then_block = Some(then_block);

            if inverted {
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce()
            }

            //Self::simplify_if(if_stat);

            self.function.set_edges(
                entry,
                vec![(else_node, BlockEdge::new(BranchType::Unconditional))],
            );

            self.match_jump(entry, Some(else_node), dominators);

            true
        };

        _match_triangle_conditional(then_node, else_node, false)
            || _match_triangle_conditional(else_node, then_node, true)
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
    ) -> bool {
        let mut changed = false;
        let header_successors = self.function.successor_blocks(header).collect_vec();
        let block = self.function.block_mut(entry).unwrap();
        if let Some(if_stat) = block.last_mut().unwrap().as_if_mut() {
            if then_node == header && !header_successors.contains(&entry) {
                if_stat.then_block = Some(vec![ast::Continue {}.into()].into());
                changed = true;
            } else if then_node == next {
                if_stat.then_block = Some(vec![ast::Break {}.into()].into());
                changed = true;
            }
            if else_node == header && !header_successors.contains(&entry) {
                if_stat.else_block = Some(vec![ast::Continue {}.into()].into());
                changed = true;
            } else if else_node == next {
                if_stat.else_block = Some(vec![ast::Break {}.into()].into());
                changed = true;
            }
            if if_stat.then_block.is_some() && if_stat.else_block.is_none() {
                self.function.set_edges(
                    entry,
                    vec![(else_node, BlockEdge::new(BranchType::Unconditional))],
                );
                changed = true;
            } else if if_stat.then_block.is_none() && if_stat.else_block.is_some() {
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce();
                std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
                self.function.set_edges(
                    entry,
                    vec![(then_node, BlockEdge::new(BranchType::Unconditional))],
                );
                changed = true;
            } else if if_stat.then_block.is_some() && if_stat.else_block.is_some() {
                self.function.remove_edges(entry);
                changed = true;
            }
        }
        changed
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
        if block.last_mut().unwrap().as_if_mut().is_none() {
            // for loops
            return false;
        }

        self.match_diamond_conditional(entry, then_node, else_node, dominators)
            || self.match_triangle_conditional(entry, then_node, else_node, dominators)
    }
}
