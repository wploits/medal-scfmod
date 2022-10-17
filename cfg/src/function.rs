use ast::{local_allocator::LocalAllocator, LocalRw, RcLocal};
use contracts::requires;
use fxhash::FxHashMap;
use itertools::Itertools;
use petgraph::{
    stable_graph::{EdgeReference, Neighbors, NodeIndex, StableDiGraph},
    visit::{EdgeRef, IntoEdgesDirected},
    Direction,
};
use std::{cell::RefCell, rc::Rc};

use crate::block::{BlockEdge, BranchType};

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub local_allocator: Rc<RefCell<LocalAllocator>>,
    pub parameters: Vec<RcLocal>,
    pub upvalues_captured: Vec<RcLocal>,
    // TODO: edge data in graph instead of terminator (arguments)
    graph: StableDiGraph<ast::Block, BlockEdge>,
    /*pub upvalue_open_ranges:
    FxHashMap<ValueId, FxHashMap<InstructionLocation, Vec<InstructionLocation>>>,*/
    entry: Option<NodeIndex>,
}

impl Function {
    pub fn with_allocator(local_allocator: Rc<RefCell<LocalAllocator>>) -> Self {
        Self {
            local_allocator,
            parameters: Vec::new(),
            upvalues_captured: Vec::new(),
            graph: StableDiGraph::new(),
            entry: None,
        }
    }

    pub fn entry(&self) -> &Option<NodeIndex> {
        &self.entry
    }

    #[requires(self.has_block(new_entry))]
    pub fn set_entry(&mut self, new_entry: NodeIndex) {
        self.entry = Some(new_entry);
    }

    // #[requires(self.has_block(block))]
    // pub fn set_block_terminator(
    //     &mut self,
    //     block: NodeIndex,
    //     mut new_terminator: Option<Terminator>,
    // ) {
    //     self.graph
    //         .retain_edges(|g, e| g.edge_endpoints(e).unwrap().0 != block);
    //     match &new_terminator {
    //         Some(Terminator::Jump(edge)) => {
    //             self.graph.add_edge(block, edge.node, ());
    //         }
    //         Some(Terminator::Conditional(then_edge, else_edge)) => {
    //             self.graph.add_edge(block, then_edge.node, ());
    //             if then_edge.node != else_edge.node {
    //                 self.graph.add_edge(block, else_edge.node, ());
    //             } else {
    //                 new_terminator = Some(Terminator::jump(then_edge.node));
    //             }

    //             /*lif then_edge.node != else_edge.node {
    //                 self.graph.add_edge(block, else_edge.node, ());
    //             } else {
    //                 et source_block = &mut self.block_mut(block).unwrap().ast;
    //                 let condition = source_block.pop().unwrap().into_if().unwrap().condition;
    //                 if condition.has_side_effects() {
    //                     let local = self.local_allocator.borrow_mut().allocate();
    //                     self.block_mut(block)
    //                         .unwrap()
    //                         .ast
    //                         .push(ast::Assign::new(vec![local.into()], vec![condition]).into());
    //                 }
    //                 new_terminator = Some(Terminator::jump(then_edge.node));
    //             }*/
    //         }
    //         _ => {}
    //     }
    //     self.block_mut(block).unwrap().terminator = new_terminator;
    // }

    // #[requires(self.graph.find_edge(source, old_target).is_some())]
    // #[requires(self.has_block(target))]
    // pub fn replace_edge(&mut self, source: NodeIndex, old_target: NodeIndex, target: NodeIndex) {
    //     if self.successor_blocks(source).contains(&target) {
    //         self.set_block_terminator(source, Some(Terminator::jump(target)));
    //     } else {
    //         self.graph
    //             .remove_edge(self.graph.find_edge(source, old_target).unwrap());
    //         self.graph.add_edge(source, target, ());
    //         self.block_mut(source)
    //             .unwrap()
    //             .terminator
    //             .as_mut()
    //             .unwrap()
    //             .replace_branch(old_target, target);
    //     }
    // }

    // TODO: take EdgeIndex as argument
    // pub fn remove_edge(&mut self, source: NodeIndex, target: NodeIndex) {
    //     self.graph
    //         .remove_edge(self.graph.find_edge(source, target).unwrap());
    // }

    pub fn graph(&self) -> &StableDiGraph<ast::Block, BlockEdge> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut StableDiGraph<ast::Block, BlockEdge> {
        &mut self.graph
    }

    pub fn has_block(&self, block: NodeIndex) -> bool {
        self.graph.contains_node(block)
    }

    pub fn block(&self, block: NodeIndex) -> Option<&ast::Block> {
        self.graph.node_weight(block)
    }

    pub fn block_mut(&mut self, block: NodeIndex) -> Option<&mut ast::Block> {
        self.graph.node_weight_mut(block)
    }

    pub fn blocks(&self) -> impl Iterator<Item = (NodeIndex, &ast::Block)> {
        self.graph
            .node_indices()
            .map(|i| (i, self.graph.node_weight(i).unwrap()))
    }

    // TODO: indexmap
    pub fn blocks_mut(&mut self) -> FxHashMap<NodeIndex, &mut ast::Block> {
        self.graph
            .node_indices()
            .collect_vec()
            .into_iter()
            .zip(self.graph.node_weights_mut())
            .collect()
    }

    pub fn successor_blocks(&self, block: NodeIndex) -> Neighbors<BlockEdge> {
        self.graph.neighbors_directed(block, Direction::Outgoing)
    }

    pub fn predecessor_blocks(&self, block: NodeIndex) -> Neighbors<BlockEdge> {
        self.graph.neighbors_directed(block, Direction::Incoming)
    }

    pub fn edges_to_block(&self, node: NodeIndex) -> impl Iterator<Item = (NodeIndex, &BlockEdge)> {
        let mut edges = self.predecessor_blocks(node).detach();
        std::iter::from_fn(move || edges.next_edge(&self.graph)).filter_map(move |e| {
            let (source, target) = self.graph.edge_endpoints(e).unwrap();
            if target == node {
                Some((source, self.graph.edge_weight(e).unwrap()))
            } else {
                None
            }
        })
    }

    pub fn edges(&self, node: NodeIndex) -> impl Iterator<Item = EdgeReference<BlockEdge>> {
        self.graph.edges_directed(node, Direction::Outgoing)
    }

    pub fn remove_edges(&mut self, node: NodeIndex) -> Vec<(NodeIndex, BlockEdge)> {
        let mut edges = Vec::new();
        for (target, edge) in self
            .edges(node)
            .map(|e| (e.target(), e.id()))
            .collect::<Vec<_>>()
        {
            edges.push((target, self.graph.remove_edge(edge).unwrap()));
        }
        edges
    }

    // returns previous edges
    pub fn set_edges(
        &mut self,
        node: NodeIndex,
        new_edges: Vec<(NodeIndex, BlockEdge)>,
    ) -> Vec<(NodeIndex, BlockEdge)> {
        let prev_edges = self.remove_edges(node);
        for (target, edge) in new_edges {
            self.graph.add_edge(node, target, edge);
        }
        prev_edges
    }

    pub fn conditional_edges(
        &self,
        node: NodeIndex,
    ) -> Option<(EdgeReference<BlockEdge>, EdgeReference<BlockEdge>)> {
        let edges = self
            .graph
            .edges_directed(node, Direction::Outgoing)
            .collect::<Vec<_>>();
        if let [e0, e1] = edges[..] {
            let mut res = (e0, e1);
            if res.1.weight().branch_type == BranchType::Then {
                std::mem::swap(&mut res.0, &mut res.1);
            }
            assert!(res.0.weight().branch_type == BranchType::Then);
            assert!(res.1.weight().branch_type == BranchType::Else);
            Some(res)
        } else {
            None
        }
    }

    pub fn unconditional_edge(&self, node: NodeIndex) -> Option<EdgeReference<BlockEdge>> {
        let edges = self
            .graph
            .edges_directed(node, Direction::Outgoing)
            .collect::<Vec<_>>();
        if let [e] = edges[..] {
            Some(e)
        } else {
            None
        }
    }

    // TODO: disable_contracts for production builds
    #[requires(self.has_block(node))]
    pub fn values_read(&self, node: NodeIndex) -> impl Iterator<Item = &RcLocal> {
        self.block(node)
            .unwrap()
            .0
            .iter()
            .flat_map(|s| s.values_read())
            .chain(self.edges(node).flat_map(|e| {
                e.weight()
                    .arguments
                    .iter()
                    .flat_map(|(_, a)| a.values_read())
            }))
    }

    // pub fn edges_to_block_mut(&mut self, node: NodeIndex) -> Vec<&mut BasicBlockEdge> {
    //     self.graph
    //         .node_weights_mut()
    //         .filter_map(|w| w.terminator.as_mut())
    //         .flat_map(|t| t.edges_mut())
    //         .filter(|e| e.node == node)
    //         .collect()
    // }

    // #[requires(self.graph.find_edge(source, target).is_some())]
    // pub fn edge(&self, source: NodeIndex, target: NodeIndex) -> Option<&BasicBlockEdge> {
    //     match self.block(source).unwrap().terminator.as_ref().unwrap() {
    //         Terminator::Jump(edge) => Some(edge),
    //         Terminator::Conditional(then_edge, else_edge) if then_edge.node == target => {
    //             Some(then_edge)
    //         }
    //         Terminator::Conditional(then_edge, else_edge) if else_edge.node == target => {
    //             Some(else_edge)
    //         }
    //         _ => None,
    //     }
    // }

    // #[requires(self.graph.find_edge(source, target).is_some())]
    // pub fn edge_mut(
    //     &mut self,
    //     source: NodeIndex,
    //     target: NodeIndex,
    // ) -> Option<&mut BasicBlockEdge> {
    //     match self.block_mut(source).unwrap().terminator.as_mut().unwrap() {
    //         Terminator::Jump(edge) => Some(edge),
    //         Terminator::Conditional(then_edge, _) if then_edge.node == target => Some(then_edge),
    //         Terminator::Conditional(_, else_edge) if else_edge.node == target => Some(else_edge),
    //         _ => None,
    //     }
    // }

    pub fn new_block(&mut self) -> NodeIndex {
        self.graph.add_node(ast::Block::default())
    }

    pub fn remove_block(&mut self, block: NodeIndex) -> Option<ast::Block> {
        self.graph.remove_node(block)
    }
}
