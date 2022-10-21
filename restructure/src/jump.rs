use ast::SideEffects;
use cfg::block::{BlockEdge, BranchType};
use itertools::Itertools;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::EdgeRef, Direction};

impl super::GraphStructurer {
    pub(crate) fn match_jump(
        &mut self,
        node: NodeIndex,
        target: Option<NodeIndex>,
        dominators: &Dominators<NodeIndex>,
    ) -> bool {
        if let Some(target) = target {
            assert!(self.function.unconditional_edge(node).is_some());
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
                    let block = self.function.block(source).unwrap();
                    if !block.is_empty()
                        && block.last().unwrap().as_if().is_some()
                        && let Some((then_edge, else_edge)) = self.function.conditional_edges(source)
                        && then_edge.target() == else_edge.target()
                    {
                        // TODO: check if this works (+ cfg/src/ssa/structuring.rs)
                        let cond = self
                            .function
                            .block_mut(source)
                            .unwrap()
                            .pop()
                            .unwrap()
                            .into_if()
                            .unwrap()
                            .condition;
                        if cond.has_side_effects() {
                            let temp_local = self.function.local_allocator.borrow_mut().allocate();
                            self.function.block_mut(node).unwrap().push(
                                ast::Assign {
                                    left: vec![temp_local.into()],
                                    right: vec![cond],
                                    prefix: true,
                                    parallel: false,
                                }
                                .into(),
                            )
                        }

                        self.function.set_edges(
                            source,
                            vec![(target, BlockEdge::new(BranchType::Unconditional))],
                        );
                    }
                }
                self.function.remove_block(node);
                true
            } else if self.function.predecessor_blocks(target).count() == 1
                // TODO: isnt this implied by their only being one predecessor, target?
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
