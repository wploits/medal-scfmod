use ast::{local_allocator::LocalAllocator, RcLocal};
use contracts::requires;
use fxhash::{FxHashMap, FxHashSet};
use graph::{Directed, Edge, Graph, NodeId};
use std::{cell::RefCell, rc::Rc};

use crate::block::{BasicBlock, BlockEdge, Edges};

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub local_allocator: Rc<RefCell<LocalAllocator>>,
    pub parameters: Vec<RcLocal>,
    pub upvalues_captured: Vec<RcLocal>,
    graph: Graph<Directed>,
    pub blocks: FxHashMap<NodeId, BasicBlock>,
    entry: Option<NodeId>,
}

impl Function {
    pub fn new(local_allocator: Rc<RefCell<LocalAllocator>>) -> Self {
        Self {
            local_allocator,
            ..Default::default()
        }
    }

    pub fn entry(&self) -> &Option<NodeId> {
        &self.entry
    }

    #[requires(self.has_block(new_entry))]
    pub fn set_entry(&mut self, new_entry: NodeId) {
        self.entry = Some(new_entry);
    }

    #[requires(self.has_block(block_id))]
    pub fn set_block_terminator(&mut self, block_id: NodeId, new_terminator: Option<Edges>) {
        for edge in &self.graph.edges().clone() {
            if edge.0 == block_id {
                self.graph.remove_edge(edge);
            }
        }
        match &new_terminator {
            Some(Edges::Jump(edge)) => {
                self.graph.add_edge((block_id, edge.node));
            }
            Some(Edges::Conditional(then_edge, else_edge)) => {
                self.graph.add_edge((block_id, then_edge.node));
                self.graph.add_edge((block_id, else_edge.node));
            }
            _ => {}
        }
        self.block_mut(block_id).unwrap().terminator = new_terminator;
    }

    #[requires(self.graph.has_edge(old) && self.has_block(target))]
    pub fn replace_edge(&mut self, old: &Edge, target: NodeId) {
        self.graph.remove_edge(old);
        self.graph.add_edge((old.0, target));
        self.block_mut(old.0)
            .unwrap()
            .terminator
            .as_mut()
            .unwrap()
            .replace_branch(old.1, target);
    }

    pub fn remove_edge(&mut self, edge: &Edge) {
        self.graph.remove_edge(edge);
    }

    pub fn graph(&self) -> &Graph<Directed> {
        &self.graph
    }

    pub fn has_block(&self, block: NodeId) -> bool {
        self.graph.has_node(block)
    }

    pub fn block(&self, block: NodeId) -> Option<&BasicBlock> {
        self.blocks.get(&block)
    }

    pub fn block_mut(&mut self, block: NodeId) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&block)
    }

    pub fn blocks(&self) -> &FxHashMap<NodeId, BasicBlock> {
        &self.blocks
    }

    pub fn blocks_mut(&mut self) -> &mut FxHashMap<NodeId, BasicBlock> {
        &mut self.blocks
    }

    pub fn predecessor_blocks(&self, block: NodeId) -> Vec<&BasicBlock> {
        self.graph
            .predecessors(block)
            .into_iter()
            .map(|n| &self.blocks()[&n])
            .collect::<Vec<_>>()
    }

    pub fn edges_to_block(&self, node: NodeId) -> Vec<&BlockEdge> {
        self.predecessor_blocks(node)
            .into_iter()
            .flat_map(|block| match &block.terminator {
                Some(Edges::Jump(edge)) => vec![edge],
                Some(Edges::Conditional(then_edge, else_edge)) => vec![then_edge, else_edge],
                _ => vec![],
            })
            .filter(|edge| edge.node == node)
            .collect::<Vec<_>>()
    }

    pub fn new_block(&mut self) -> NodeId {
        let node = self.graph.add_node();
        self.blocks.insert(node, BasicBlock::default());
        node
    }

    pub fn remove_block(&mut self, block: NodeId) -> Option<BasicBlock> {
        let removed_block = self.blocks.remove(&block);
        if removed_block.is_some() {
            self.graph.remove_node(block);
        }
        removed_block
    }
}
