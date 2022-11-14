use ast::Reduce;
use cfg::block::{BlockEdge, BranchType};
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

    fn expand_if(if_stat: &mut ast::If) -> Option<ast::Block> {
        let then_return = if_stat.then_block.last().and_then(|x| x.as_return());
        let else_return = if_stat.else_block.last().and_then(|x| x.as_return());
        if let Some(then_return) = then_return && let Some(else_return) = else_return {
            if then_return.values.is_empty() && else_return.values.is_empty() {
                if_stat.then_block.pop();
                if_stat.else_block.pop();
                None
            } else if !then_return.values.is_empty() && else_return.values.is_empty() {
                Some(std::mem::take(&mut if_stat.else_block))
            } else if then_return.values.is_empty() && !else_return.values.is_empty() {
                let then_block = std::mem::replace(&mut if_stat.then_block, std::mem::take(&mut if_stat.else_block));
                // TODO: unnecessary clone (also other cases)
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce_condition();
                Some(then_block)
            } else {
                match if_stat.then_block.len().cmp(&if_stat.else_block.len()) {
                    std::cmp::Ordering::Less => Some(std::mem::take(&mut if_stat.else_block)),
                    std::cmp::Ordering::Greater => {
                        let then_block = std::mem::replace(&mut if_stat.then_block, std::mem::take(&mut if_stat.else_block));
                        if_stat.condition =
                            ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not)
                                .reduce_condition();
                        Some(then_block)
                    }
                    // TODO: `Some(std::mem::take(&mut if_stat.else_block))`?
                    std::cmp::Ordering::Equal => None,
                }
            }
        } else {
            None
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
        // TODO: STYLE: rename to r#if?
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = then_block;
        if_stat.else_block = else_block;
        Self::simplify_if(if_stat);

        let after = Self::expand_if(if_stat);
        if if_stat.then_block.is_empty() {
            // TODO: unnecessary clone
            if_stat.condition =
                ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce_condition();
            std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
        }
        if let Some(after) = after {
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

            if then_successors.len() > 1 {
                return false;
            }

            if self.function.predecessor_blocks(then_node).count() != 1 {
                return false;
            }

            if !then_successors.is_empty() && then_successors[0] != else_node {
                return false;
            }

            let then_block = self.function.remove_block(then_node).unwrap();

            let block = self.function.block_mut(entry).unwrap();
            let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
            if_stat.then_block = then_block;

            if inverted {
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce_condition()
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
        post_dom: &Dominators<NodeIndex>,
        entry: NodeIndex,
        node: NodeIndex,
        header: NodeIndex,
        next: NodeIndex,
    ) -> bool {
        if node == header {
            // TODO: only check back edges?
            if !self
                .function
                .predecessor_blocks(header)
                .filter(|&n| n != entry)
                .any(|n| {
                    post_dom
                        .dominators(entry)
                        .is_some_and(|mut p| p.contains(&n))
                })
            {
                return false;
            }
            let block = &mut self.function.block_mut(entry).unwrap();
            block.push(ast::Continue {}.into());
        } else if node == next {
            let block = &mut self.function.block_mut(entry).unwrap();
            block.push(ast::Break {}.into());
        }
        self.function.set_edges(entry, vec![]);
        true
    }

    pub(crate) fn refine_virtual_edge_conditional(
        &mut self,
        post_dom: &Dominators<NodeIndex>,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
        header: NodeIndex,
        next: NodeIndex,
    ) -> bool {
        let then_main_cont = self
            .function
            .predecessor_blocks(header)
            .filter(|&n| n != entry)
            .any(|n| {
                post_dom
                    .dominators(then_node)
                    .is_some_and(|mut p| p.contains(&n))
            });

        let else_main_cont = self
            .function
            .predecessor_blocks(header)
            .filter(|&n| n != entry)
            .any(|n| {
                post_dom
                    .dominators(else_node)
                    .is_some_and(|mut p| p.contains(&n))
            });

        let mut changed = false;
        let header_successors = self.function.successor_blocks(header).collect_vec();
        let block = self.function.block_mut(entry).unwrap();
        if let Some(if_stat) = block.last_mut().unwrap().as_if_mut() {
            if then_node == header && !header_successors.contains(&entry) && then_main_cont {
                if_stat.then_block = vec![ast::Continue {}.into()].into();
                changed = true;
            } else if then_node == next {
                if_stat.then_block = vec![ast::Break {}.into()].into();
                changed = true;
            }
            if else_node == header && !header_successors.contains(&entry) && else_main_cont {
                if_stat.else_block = vec![ast::Continue {}.into()].into();
                changed = true;
            } else if else_node == next {
                if_stat.else_block = vec![ast::Break {}.into()].into();
                changed = true;
            }
            if !if_stat.then_block.is_empty() && if_stat.else_block.is_empty() {
                self.function.set_edges(
                    entry,
                    vec![(else_node, BlockEdge::new(BranchType::Unconditional))],
                );
                changed = true;
            } else if if_stat.then_block.is_empty() && !if_stat.else_block.is_empty() {
                if_stat.condition =
                    ast::Unary::new(if_stat.condition.clone(), ast::UnaryOperation::Not).reduce_condition();
                std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
                self.function.set_edges(
                    entry,
                    vec![(then_node, BlockEdge::new(BranchType::Unconditional))],
                );
                changed = true;
            } else if !if_stat.then_block.is_empty() && !if_stat.else_block.is_empty() {
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
        let block = self.function.block_mut(entry).unwrap();
        if block.last_mut().unwrap().as_if_mut().is_none() {
            // for loops
            return false;
        }

        self.match_diamond_conditional(entry, then_node, else_node, dominators)
            || self.match_triangle_conditional(entry, then_node, else_node, dominators)
    }
}
