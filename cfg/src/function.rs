use ast::{local_allocator::LocalAllocator, RcLocal};
use contracts::requires;
use fxhash::FxHashMap;
use graph::{Edge, Graph, NodeId};

use crate::block::{BasicBlock, Terminator};

#[derive(Debug, Clone, Default)]
pub struct Function<'a> {
    pub local_allocator: LocalAllocator,
    pub parameters: Vec<RcLocal<'a>>,
    graph: Graph,
    blocks: FxHashMap<NodeId, BasicBlock<'a>>,
    /*pub upvalue_open_ranges:
    FxHashMap<ValueId, FxHashMap<InstructionLocation, Vec<InstructionLocation>>>,*/
    entry: Option<NodeId>,
}

impl<'a> Function<'a> {
    pub fn entry(&self) -> &Option<NodeId> {
        &self.entry
    }

    #[requires(self.has_block(new_entry))]
    pub fn set_entry(&mut self, new_entry: NodeId) {
        self.entry = Some(new_entry);
    }

    #[requires(self.has_block(block_id))]
    pub fn set_block_terminator(
        &mut self,
        block_id: NodeId,
        new_terminator: Option<Terminator<'a>>,
    ) {
        for edge in &self.graph.edges().clone() {
            if edge.0 == block_id {
                self.graph.remove_edge(edge);
            }
        }
        match &new_terminator {
            Some(Terminator::Jump(edge)) => {
                self.graph.add_edge((block_id, edge.node));
            }
            Some(Terminator::Conditional(then_edge, else_edge)) => {
                self.graph.add_edge((block_id, then_edge.node));
                self.graph.add_edge((block_id, else_edge.node));
            }
            _ => {}
        }
        if let Some(new_terminator) = new_terminator {
            self.block_mut(block_id)
                .unwrap()
                .terminator
                .replace(new_terminator);
        }
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

    pub fn graph(&self) -> &Graph {
        &self.graph
    }

    pub fn has_block(&self, block: NodeId) -> bool {
        self.graph.has_node(block)
    }

    pub fn block(&self, block: NodeId) -> Option<&BasicBlock<'a>> {
        self.blocks.get(&block)
    }

    pub fn block_mut(&mut self, block: NodeId) -> Option<&mut BasicBlock<'a>> {
        self.blocks.get_mut(&block)
    }

    pub fn blocks(&self) -> &FxHashMap<NodeId, BasicBlock<'a>> {
        &self.blocks
    }

    pub fn blocks_mut(&mut self) -> &mut FxHashMap<NodeId, BasicBlock<'a>> {
        &mut self.blocks
    }

    pub fn new_block(&mut self) -> NodeId {
        let node = self.graph.add_node();
        self.blocks.insert(node, BasicBlock::default());
        node
    }

    pub fn remove_block(&mut self, block: NodeId) -> Option<BasicBlock<'a>> {
        let removed_block = self.blocks.remove(&block);
        if removed_block.is_some() {
            self.graph.remove_node(block);
        }
        removed_block
    }
}
