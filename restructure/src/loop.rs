use ast::{Reduce, SideEffects, Traverse};
use cfg::block::{BlockEdge, BranchType};
use fxhash::FxHashSet;
use itertools::Itertools;

use crate::{post_dominators, GraphStructurer};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::EdgeRef};

impl GraphStructurer {
    pub(crate) fn is_loop_header(&self, node: NodeIndex) -> bool {
        self.loop_headers.contains(&node)
    }

    pub(crate) fn try_collapse_loop(
        &mut self,
        header: NodeIndex,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        if !self.is_loop_header(header) {
            return false;
        }

        let successors = self.function.successor_blocks(header).collect::<Vec<_>>();
        if successors.contains(&header)
            && !self
                .function
                .block(header)
                .unwrap()
                .last()
                .map(|s| s.as_num_for_next().is_some() || s.as_generic_for_next().is_some())
                .unwrap_or(false)
        {
            if successors.len() == 2 {
                let header_block = self.function.block_mut(header).unwrap();
                let if_stat = header_block.pop().unwrap().into_if().unwrap();
                let mut condition = if_stat.condition;
                let (then_edge, else_edge) = self.function.conditional_edges(header).unwrap();
                let next = if then_edge.target() == header {
                    condition = ast::Unary::new(condition, ast::UnaryOperation::Not).reduce();
                    else_edge.target()
                } else {
                    then_edge.target()
                };
                let header_block = self.function.block_mut(header).unwrap();
                *header_block =
                    vec![ast::Repeat::new(condition, header_block.clone()).into()].into();
                self.function.set_edges(
                    header,
                    vec![(next, BlockEdge::new(BranchType::Unconditional))],
                );
            } else {
                let header_block = self.function.block_mut(header).unwrap();
                *header_block =
                    vec![
                        ast::While::new(ast::Literal::Boolean(true).into(), header_block.clone())
                            .into(),
                    ]
                    .into();
                self.function.remove_edges(header);
            }
            true
        } else if successors.len() == 2 {
            let post_dom = post_dominators(self.function.graph_mut());
            let (mut next, mut body) = (successors[0], successors[1]);
            if post_dom.immediate_dominator(header) == Some(body) {
                std::mem::swap(&mut next, &mut body);
            }

            //println!("{:?} {:?}", next, body);

            if self
                .function
                .predecessor_blocks(body)
                .filter(|&p| p != body)
                .count()
                != 1
            {
                return false;
            }

            /*let latches = self
                .function
                .graph()
                .neighbors_directed(header, Direction::Incoming)
                .filter(|&n| n != next && dominators.dominators(n).unwrap().contains(&header))
                .collect_vec();
            let breaks = self
                .function
                .graph()
                .neighbors_directed(next, Direction::Incoming)
                .filter(|&n| n != header && dominators.dominators(n).unwrap().contains(&header))
                .collect_vec();

            println!("latches: {:#?}", latches);
            println!("breaks: {:#?}", breaks);*/

            let breaks = self
                .function
                .predecessor_blocks(next)
                .filter(|&n| n != header)
                .filter(|&n| dominators.dominators(n).unwrap().contains(&header))
                .collect_vec();

            let continues = self
                .function
                .predecessor_blocks(header)
                // TODO: the line below fixes `for i = 1, 10 do end`, but a different approach might be preferable
                .filter(|&n| n != header)
                .filter(|&n| dominators.dominators(n).unwrap().contains(&header))
                .collect_vec();
            //assert!(continues.len() <= 1);
            //println!("continues: {:?}", continues);

            let mut changed = false;

            for node in breaks
                .into_iter()
                .chain(continues)
                .collect::<FxHashSet<_>>()
            {
                if let Some((then_edge, else_edge)) = self.function.conditional_edges(node) {
                    changed |= self.refine_virtual_edge_conditional(
                        node,
                        then_edge.target(),
                        else_edge.target(),
                        header,
                        next,
                    );
                } else if let Some(edge) = self.function.unconditional_edge(node) {
                    changed |= self.refine_virtual_edge_jump(node, edge.target(), header, next);
                } else {
                    unreachable!();
                }
            }

            if body == header
                || self.function.successor_blocks(body).exactly_one().ok() == Some(header)
            {
                let statement = self.function.block_mut(header).unwrap().pop().unwrap();
                if let ast::Statement::If(if_stat) = statement {
                    let mut if_condition = if_stat.condition;
                    let header_else_target =
                        self.function.conditional_edges(header).unwrap().1.target();
                    let block = if body == header {
                        unimplemented!()
                    } else {
                        self.function.remove_block(body).unwrap()
                    };

                    let while_stat = if !self.function.block_mut(header).unwrap().is_empty() {
                        let mut body_block =
                            std::mem::take(self.function.block_mut(header).unwrap());
                        if header_else_target != body {
                            // TODO: is this correct?
                            if_condition =
                                ast::Unary::new(if_condition, ast::UnaryOperation::Not).reduce();
                        }
                        body_block.push(
                            ast::If::new(
                                if_condition,
                                Some(vec![ast::Break {}.into()].into()),
                                None,
                            )
                            .into(),
                        );
                        body_block.extend(block.0.into_iter());

                        ast::While::new(ast::Literal::Boolean(true).into(), body_block)
                    } else {
                        if header_else_target == body {
                            if_condition =
                                ast::Unary::new(if_condition, ast::UnaryOperation::Not).reduce();
                        }

                        ast::While::new(if_condition, block)
                    };

                    self.function
                        .block_mut(header)
                        .unwrap()
                        .push(while_stat.into());
                    self.function.set_edges(
                        header,
                        vec![(next, BlockEdge::new(BranchType::Unconditional))],
                    );
                    self.match_jump(header, Some(next), dominators);

                    return true;
                } else if self.function.block_mut(header).unwrap().is_empty() {
                    if let ast::Statement::NumForNext(num_for_next) = statement {
                        let predecessors = self
                            .function
                            .predecessor_blocks(header)
                            .filter(|&p| p != header)
                            .collect_vec();
                        let mut init_blocks = predecessors.into_iter().filter_map(|p| {
                            self.function
                                .block_mut(p)
                                .unwrap()
                                .iter_mut()
                                .enumerate()
                                .rev()
                                .find(|(_, s)| {
                                    s.has_side_effects() || s.as_num_for_init().is_some()
                                })
                                .and_then(|(i, s)| s.as_num_for_init_mut().map(|_| (p, i)))
                        });
                        let (init_block, init_index) = init_blocks.next().unwrap();
                        assert!(init_blocks.next().is_none());
                        let body_ast = if body == header {
                            ast::Block::default()
                        } else {
                            self.function.remove_block(body).unwrap()
                        };
                        let init_ast = &mut self.function.block_mut(init_block).unwrap();
                        let for_init = init_ast.remove(init_index).into_num_for_init().unwrap();
                        let numeric_for = ast::NumericFor::new(
                            for_init.counter.1,
                            for_init.limit.1,
                            for_init.step.1,
                            num_for_next.counter.0.as_local().unwrap().clone(),
                            body_ast,
                        );
                        init_ast.push(numeric_for.into());
                        self.function.remove_block(header);
                        self.function.set_edges(
                            init_block,
                            vec![(next, BlockEdge::new(BranchType::Unconditional))],
                        );
                        self.match_jump(init_block, Some(next), dominators);
                        return true;
                    } else if let ast::Statement::GenericForNext(generic_for_next) = statement {
                        let predecessors = self
                            .function
                            .predecessor_blocks(header)
                            .filter(|&p| p != header)
                            .collect_vec();
                        let mut init_blocks = predecessors.into_iter().filter_map(|p| {
                            self.function
                                .block_mut(p)
                                .unwrap()
                                .iter_mut()
                                .enumerate()
                                .rev()
                                .find(|(_, s)| {
                                    s.has_side_effects() || s.as_generic_for_init().is_some()
                                })
                                .and_then(|(i, s)| s.as_generic_for_init().map(|_| (p, i)))
                        });
                        let (init_block, init_index) = init_blocks.next().unwrap();
                        assert!(init_blocks.next().is_none());
                        let body_ast = if body == header {
                            ast::Block::default()
                        } else {
                            self.function.remove_block(body).unwrap()
                        };
                        let init_ast = self.function.block_mut(init_block).unwrap();
                        let for_init = init_ast.remove(init_index).into_generic_for_init().unwrap();
                        let generic_for = ast::GenericFor::new(
                            generic_for_next
                                .res_locals
                                .iter()
                                .map(|l| l.as_local().unwrap().clone())
                                .collect(),
                            for_init.0.right,
                            body_ast,
                        );
                        init_ast.push(generic_for.into());
                        self.function.remove_block(header);
                        self.function.set_edges(
                            init_block,
                            vec![(next, BlockEdge::new(BranchType::Unconditional))],
                        );
                        self.match_jump(init_block, Some(next), dominators);
                        return true;
                    }
                }
            }
            changed
        } else {
            false
        }
    }
}
