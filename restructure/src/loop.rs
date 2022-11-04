use ast::{Reduce, SideEffects};
use cfg::block::{BlockEdge, BranchType};
use itertools::Itertools;
use rustc_hash::FxHashSet;

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

            let mut changed = false;
            // if body == header then body dominates next, which results in breaks being inserted
            // outside the loop
            if body != header {
                let breaks = self
                    .function
                    .predecessor_blocks(next)
                    .filter(|&n| n != header)
                    .filter(|&n| dominators.dominators(n).unwrap().contains(&body))
                    .collect_vec();
                //println!("breaks: {:?}", breaks);

                let continues = self
                    .function
                    .predecessor_blocks(header)
                    // TODO: the line below fixes `for i = 1, 10 do end`, but a different approach might be preferable
                    .filter(|&n| n != header)
                    .filter(|&n| {
                        dominators
                            .dominators(n)
                            .map(|mut x| x.contains(&body))
                            .unwrap_or(false)
                    })
                    .collect_vec();
                //assert!(continues.len() <= 1);
                //println!("continues: {:?}", continues);

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
                        changed |= self.refine_virtual_edge_jump(
                            &post_dom,
                            node,
                            edge.target(),
                            header,
                            next,
                        );
                    } else {
                        unreachable!();
                    }
                }
            }
            //println!("changed: {:?}", changed);

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
                                vec![ast::Break {}.into()].into(),
                                ast::Block::default(),
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
                } else {
                    let statements =
                        std::mem::take(&mut self.function.block_mut(header).unwrap().0);

                    let predecessors = self
                        .function
                        .predecessor_blocks(header)
                        .filter(|&p| p != header)
                        .collect_vec();
                    let init_blocks = predecessors.into_iter().filter_map(|p| {
                        self.function
                            .block_mut(p)
                            .unwrap()
                            .iter_mut()
                            .enumerate()
                            .rev()
                            // TODO: REFACTOR: this is confusing
                            .find(|(_, s)| {
                                s.has_side_effects()
                                    || s.as_num_for_init().is_some()
                                    || s.as_generic_for_init().is_some()
                            })
                            .and_then(|(i, s)| {
                                if s.as_num_for_init().is_some()
                                    || s.as_generic_for_init().is_some()
                                {
                                    Some((p, i))
                                } else {
                                    None
                                }
                            })
                    });
                    let (init_block, init_index) = init_blocks.exactly_one().unwrap();

                    // if !statements.is_empty() {
                    //     println!("{}", self.function.block(init_block).unwrap());
                    //     println!("--");
                    // }

                    let mut body_ast = if body == header {
                        ast::Block::default()
                    } else {
                        self.function.remove_block(body).unwrap()
                    };
                    body_ast.extend(statements.iter().cloned());
                    let init_ast = &mut self.function.block_mut(init_block).unwrap();
                    init_ast.extend(statements.iter().cloned());
                    let new_stat = match statement {
                        ast::Statement::NumForNext(num_for_next) => {
                            let for_init = init_ast.remove(init_index).into_num_for_init().unwrap();
                            ast::NumericFor::new(
                                for_init.counter.1,
                                for_init.limit.1,
                                for_init.step.1,
                                num_for_next.counter.0.as_local().unwrap().clone(),
                                body_ast,
                            )
                            .into()
                        }
                        ast::Statement::GenericForNext(generic_for_next) => {
                            let for_init =
                                init_ast.remove(init_index).into_generic_for_init().unwrap();
                            ast::GenericFor::new(
                                generic_for_next
                                    .res_locals
                                    .iter()
                                    .map(|l| l.as_local().unwrap().clone())
                                    .collect(),
                                for_init.0.right,
                                body_ast,
                            )
                            .into()
                        }
                        _ => {
                            unreachable!();
                        }
                    };
                    init_ast.push(new_stat);
                    self.function.remove_block(header);

                    // TODO: REFACTOR: make a seperate function that set_edges unconditional
                    // and calls match_jump
                    self.function.set_edges(
                        init_block,
                        vec![(next, BlockEdge::new(BranchType::Unconditional))],
                    );
                    self.match_jump(init_block, Some(next), dominators);
                    return true;
                }
            }
            changed
        } else {
            false
        }
    }
}
