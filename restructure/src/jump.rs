use ast::SideEffects;
use cfg::block::{BlockEdge, BranchType};
use itertools::Itertools;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::EdgeRef, Direction};

impl super::GraphStructurer {
    // TODO: STYLE: better name
    // TODO: this is the same as in structuring.rs but w/o block params
    // maybe we can use the same function?
    pub(crate) fn try_remove_unnecessary_condition(&mut self, node: NodeIndex) -> bool {
        let block = self.function.block(node).unwrap();
        if !block.is_empty()
            && block.last().unwrap().as_if().is_some()
            && let Some((then_edge, else_edge)) = self.function.conditional_edges(node)
            && then_edge.target() == else_edge.target()
        {
            let target = then_edge.target();
            let cond = self
                .function
                .block_mut(node)
                .unwrap()
                .pop()
                .unwrap()
                .into_if()
                .unwrap()
                .condition;

            let new_stat = match cond {
                ast::RValue::Call(call) => Some(call.into()),
                ast::RValue::MethodCall(method_call) => Some(method_call.into()),
                cond if cond.has_side_effects() => {
                    let temp_local = self.function.local_allocator.borrow_mut().allocate();
                    Some(ast::Assign {
                            left: vec![temp_local.into()],
                            right: vec![cond],
                            prefix: true,
                            parallel: false,
                        }
                        .into(),
                    )
                },
                _ => None,
            };
            self.function.block_mut(node).unwrap().extend(new_stat);
            self.function.set_edges(
                node,
                vec![(target, BlockEdge::new(BranchType::Unconditional))],
            );
            true
        } else {
            false
        }
    }

    pub(crate) fn match_jump(
        &mut self,
        node: NodeIndex,
        target: Option<NodeIndex>,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        if let Some(target) = target {
            assert!(self.function.unconditional_edge(node).is_some());
            assert!(!self.is_loop_header(target));
            if node == target {
                return false;
            }
            if Self::block_is_no_op(self.function.block(node).unwrap()) {
                for (source, edge) in self
                    .function
                    .graph()
                    .edges_directed(node, Direction::Incoming)
                    .map(|e| (e.source(), e.id()))
                    .collect::<Vec<_>>()
                {
                    let edge = self.function.graph_mut().remove_edge(edge).unwrap();
                    self.function.graph_mut().add_edge(source, target, edge);
                    self.try_remove_unnecessary_condition(source);
                }
                self.function.remove_block(node);
                true
            } else if self.function.predecessor_blocks(target).count() == 1
                // TODO: isnt this implied by their only being one predecessor, target?
                // there might be a back edge, but we should still be able to merge
                // as long as the back edge isnt from target -> node?
                && dominators.dominators(target).unwrap().contains(&node)
            {
                let edges = self.function.remove_edges(target);
                let block = self.function.remove_block(target).unwrap();
                self.function.block_mut(node).unwrap().extend(block.0);
                self.function.set_edges(node, edges);
                true
            } else {
                false
            }
        } else if Self::block_is_no_op(self.function.block(node).unwrap()) {
            let preds = self.function.predecessor_blocks(node).collect_vec();
            let mut invalid = false;
            for &pred in &preds {
                if self.function.successor_blocks(pred).collect_vec().len() != 1 {
                    invalid = true;
                    break;
                }
            }
            if !invalid {
                for edge in self
                    .function
                    .graph()
                    .edges_directed(node, Direction::Incoming)
                    .map(|e| e.id())
                    .collect::<Vec<_>>()
                {
                    assert_eq!(
                        self.function
                            .graph_mut()
                            .remove_edge(edge)
                            .unwrap()
                            .branch_type,
                        BranchType::Unconditional
                    );
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
