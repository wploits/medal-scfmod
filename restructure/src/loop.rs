use ast::{Reduce, SideEffects};
use cfg::block::Terminator;
use fxhash::FxHashSet;
use itertools::Itertools;
use std::iter;

use crate::{post_dominators, GraphStructurer};
use petgraph::{
    algo::dominators::{simple_fast, Dominators},
    stable_graph::{NodeIndex, StableDiGraph},
    visit::{IntoNeighbors, IntoNodeIdentifiers, Reversed},
};

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
                .ast
                .last()
                .map(|s| s.as_num_for_next().is_some() || s.as_generic_for_next().is_some())
                .unwrap_or(false)
        {
            if successors.len() == 2 {
                let header_block = &mut self.function.block_mut(header).unwrap().ast;
                let if_stat = header_block.pop().unwrap().into_if().unwrap();
                let mut condition = if_stat.condition;
                let (then_edge, else_edge) = self
                    .function
                    .block(header)
                    .unwrap()
                    .terminator
                    .as_ref()
                    .unwrap()
                    .as_conditional()
                    .unwrap();
                let next = if then_edge.node == header {
                    condition = ast::Unary::new(condition, ast::UnaryOperation::Not).reduce();
                    else_edge.node
                } else {
                    then_edge.node
                };
                let header_block = &mut self.function.block_mut(header).unwrap().ast;
                *header_block =
                    ast::Block::from_vec(vec![
                        ast::Repeat::new(condition, header_block.clone()).into()
                    ]);
                self.function
                    .set_block_terminator(header, Some(Terminator::jump(next)));
            } else {
                let header_block = &mut self.function.block_mut(header).unwrap().ast;
                *header_block = ast::Block::from_vec(vec![ast::While::new(
                    ast::Literal::Boolean(true).into(),
                    header_block.clone(),
                )
                .into()]);
                self.function.set_block_terminator(header, None);
            }
            true
        } else if successors.len() == 2 {
            // cant turn into a while loop if there are more statements in the block
            if self.function.block(header).unwrap().ast.len() > 1 {
                // todo; while true do
                return false;
            }

            let post_dom = post_dominators(self.function.graph().clone());
            let (mut next, mut body) = (successors[0], successors[1]);
            if post_dom.immediate_dominator(header) == Some(body) {
                std::mem::swap(&mut next, &mut body);
            }

            println!("{:?} {:?}", next, body);

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

            /*let mut continues = self
                .function
                .predecessor_blocks(header)
                .filter(|&n| dominators.dominators(n).unwrap().contains(&header))
                .collect_vec();
            if continues.len() == 1 {
                continues.clear();
            } else {
                todo!("remove node that dominates all");
            }*/
            let continues = Vec::new();

            println!("continues: {:?}", continues);

            let mut changed = false;

            for node in breaks
                .into_iter()
                .chain(continues)
                .collect::<FxHashSet<_>>()
            {
                match self
                    .function
                    .block(node)
                    .unwrap()
                    .terminator
                    .as_ref()
                    .unwrap()
                {
                    Terminator::Conditional(then_edge, else_edge) => {
                        changed |= self.refine_virtual_edge_conditional(
                            node,
                            then_edge.node,
                            else_edge.node,
                            header,
                            next,
                            dominators,
                        );
                    }
                    Terminator::Jump(edge) => {
                        self.refine_virtual_edge_jump(node, edge.node, header, next);
                    }
                };
            }

            let mut body_successors = self.function.successor_blocks(body);
            if body == header
                || body_successors.next() == Some(header) && body_successors.next().is_none()
            {
                let statement = self.function.block_mut(header).unwrap().ast.remove(0);
                if let ast::Statement::If(if_stat) = statement {
                    let mut if_condition = if_stat.condition;
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
                        if_condition =
                            ast::Unary::new(if_condition, ast::UnaryOperation::Not).reduce();
                    }

                    let while_stat = ast::While::new(
                        if_condition,
                        if body == header {
                            unimplemented!()
                        } else {
                            self.function.remove_block(body).unwrap().ast
                        },
                    );
                    self.function
                        .block_mut(header)
                        .unwrap()
                        .ast
                        .push(while_stat.into());
                    self.function
                        .set_block_terminator(header, Some(Terminator::jump(next)));
                    self.match_jump(header, Some(next), dominators);
                } else if let ast::Statement::NumForNext(num_for_next) = statement {
                    let predecessors = self
                        .function
                        .predecessor_blocks(header)
                        .filter(|&p| p != header)
                        .collect_vec();
                    let mut init_blocks = predecessors.into_iter().filter_map(|p| {
                        self.function
                            .block_mut(p)
                            .unwrap()
                            .ast
                            .iter_mut()
                            .enumerate()
                            .rev()
                            .find(|(_, s)| s.has_side_effects() || s.as_num_for_init().is_some())
                            .and_then(|(i, s)| s.as_num_for_init_mut().map(|_| (p, i)))
                    });
                    let (init_block, init_index) = init_blocks.next().unwrap();
                    assert!(init_blocks.next().is_none());
                    let body_ast = if body == header {
                        ast::Block::default()
                    } else {
                        self.function.remove_block(body).unwrap().ast
                    };
                    let init_ast = &mut self.function.block_mut(init_block).unwrap().ast;
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
                    self.function
                        .set_block_terminator(init_block, Some(Terminator::jump(next)));
                    self.match_jump(init_block, Some(next), dominators);
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
                            .ast
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
                        self.function.remove_block(body).unwrap().ast
                    };
                    let init_ast = &mut self.function.block_mut(init_block).unwrap().ast;
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
                    self.function
                        .set_block_terminator(init_block, Some(Terminator::jump(next)));
                    self.match_jump(init_block, Some(next), dominators);
                } else {
                    panic!()
                }
                true
            } else {
                changed
            }
        } else {
            false
        }
    }
}
