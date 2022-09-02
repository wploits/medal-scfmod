use ast::{local_allocator::LocalAllocator, RcLocal};
use contracts::requires;
use itertools::Itertools;
use petgraph::{
    stable_graph::{Neighbors, NodeIndex, StableDiGraph},
    Direction,
};
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

use crate::block::{BasicBlock, BasicBlockEdge, Terminator};

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub local_allocator: Rc<RefCell<LocalAllocator>>,
    pub parameters: Vec<RcLocal>,
    pub upvalues_captured: Vec<RcLocal>,
    // TODO: edge data in graph instead of terminator (arguments)
    graph: StableDiGraph<BasicBlock, ()>,
    /*pub upvalue_open_ranges:
    FxHashMap<ValueId, FxHashMap<InstructionLocation, Vec<InstructionLocation>>>,*/
    entry: Option<NodeIndex>,
}

impl Function {
    pub fn entry(&self) -> &Option<NodeIndex> {
        &self.entry
    }

    #[requires(self.has_block(new_entry))]
    pub fn set_entry(&mut self, new_entry: NodeIndex) {
        self.entry = Some(new_entry);
    }

    #[requires(self.has_block(block_id))]
    pub fn set_block_terminator(
        &mut self,
        block_id: NodeIndex,
        new_terminator: Option<Terminator>,
    ) {
        self.graph
            .retain_edges(|g, e| g.edge_endpoints(e).unwrap().0 != block_id);
        match &new_terminator {
            Some(Terminator::Jump(edge)) => {
                self.graph.add_edge(block_id, edge.node, ());
            }
            Some(Terminator::Conditional(then_edge, else_edge)) => {
                self.graph.add_edge(block_id, then_edge.node, ());
                self.graph.add_edge(block_id, else_edge.node, ());
            }
            _ => {}
        }
        self.block_mut(block_id).unwrap().terminator = new_terminator;
    }

    #[requires(self.graph.find_edge(source, old_target).is_some() && self.has_block(target))]
    pub fn replace_edge(&mut self, source: NodeIndex, old_target: NodeIndex, target: NodeIndex) {
        self.graph
            .remove_edge(self.graph.find_edge(source, old_target).unwrap());
        self.graph.add_edge(source, target, ());
        self.block_mut(source)
            .unwrap()
            .terminator
            .as_mut()
            .unwrap()
            .replace_branch(old_target, target);
    }

    // TODO: take EdgeIndex as argument
    pub fn remove_edge(&mut self, source: NodeIndex, target: NodeIndex) {
        self.graph
            .remove_edge(self.graph.find_edge(source, target).unwrap());
    }

    pub fn graph(&self) -> &StableDiGraph<BasicBlock, ()> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut StableDiGraph<BasicBlock, ()> {
        &mut self.graph
    }

    pub fn has_block(&self, block: NodeIndex) -> bool {
        self.graph.contains_node(block)
    }

    pub fn block(&self, block: NodeIndex) -> Option<&BasicBlock> {
        self.graph.node_weight(block)
    }

    pub fn block_mut(&mut self, block: NodeIndex) -> Option<&mut BasicBlock> {
        self.graph.node_weight_mut(block)
    }

    pub fn blocks(&self) -> impl Iterator<Item = (NodeIndex, &BasicBlock)> {
        self.graph
            .node_indices()
            .map(|i| (i, self.graph.node_weight(i).unwrap()))
    }

    pub fn blocks_mut(&mut self) -> HashMap<NodeIndex, &mut BasicBlock> {
        self.graph
            .node_indices()
            .collect_vec()
            .into_iter()
            .zip(self.graph.node_weights_mut())
            .collect()
    }

    pub fn successor_blocks(&self, block: NodeIndex) -> Neighbors<()> {
        self.graph.neighbors_directed(block, Direction::Outgoing)
    }

    pub fn predecessor_blocks(&self, block: NodeIndex) -> Neighbors<()> {
        self.graph.neighbors_directed(block, Direction::Incoming)
    }

    pub fn edges_to_block(&self, node: NodeIndex) -> Vec<&BasicBlockEdge> {
        self.predecessor_blocks(node)
            .flat_map(|block| {
                self.graph
                    .node_weight(block)
                    .unwrap()
                    .terminator
                    .as_ref()
                    .map_or_else(Vec::new, |t| t.edges())
            })
            .filter(|edge| edge.node == node)
            .collect::<Vec<_>>()
    }

    pub fn edges_to_block_mut(&mut self, node: NodeIndex) -> Vec<&mut BasicBlockEdge> {
        self.graph
            .node_weights_mut()
            .filter_map(|w| w.terminator.as_mut())
            .flat_map(|t| t.edges_mut())
            .filter(|e| e.node == node)
            .collect::<Vec<_>>()
    }

    pub fn new_block(&mut self) -> NodeIndex {
        self.graph.add_node(BasicBlock::default())
    }

    pub fn remove_block(&mut self, block: NodeIndex) -> Option<BasicBlock> {
        self.graph.remove_node(block)
    }
}
