use ast::local_allocator::LocalAllocator;
use fxhash::FxHashMap;
use graph::{Edge, Graph, NodeId};
use std::rc::Rc;

use crate::{
    block::{BasicBlock, Terminator},
    error::{Error, Result},
};

#[derive(Debug, Clone, Default)]
pub struct Function<'a> {
    pub local_allocator: LocalAllocator,
    pub parameters: Vec<Rc<ast::Local<'a>>>,
    graph: Graph,
    blocks: FxHashMap<NodeId, BasicBlock<'a>>,
    /*pub upvalue_open_ranges:
    FxHashMap<ValueId, FxHashMap<InstructionLocation, Vec<InstructionLocation>>>,*/
    entry: Option<NodeId>,
}

impl<'cfg> Function<'cfg> {
    pub fn entry(&self) -> &Option<NodeId> {
        &self.entry
    }

    pub fn set_entry(&mut self, new_entry: NodeId) -> Result<()> {
        if self.block_exists(new_entry) {
            self.entry = Some(new_entry);
            Ok(())
        } else {
            Err(Error::InvalidBlock(new_entry))
        }
    }

    pub fn set_block_terminator(
        &mut self,
        block_id: NodeId,
        new_terminator: Option<Terminator>,
    ) -> Result<()> {
        if !self.block_exists(block_id) {
            return Err(Error::InvalidBlock(block_id));
        }
        for &edge in self
            .graph
            .edges()
            .clone()
            .iter()
            .filter(|e| e.source == block_id)
        {
            self.graph.remove_edge(edge)?;
        }
        match new_terminator {
            Some(Terminator::Jump(target)) => {
                self.graph.add_edge(Edge::new(block_id, target))?;
            }
            Some(Terminator::Conditional(then_branch, else_branch)) => {
                self.graph.add_edge(Edge::new(block_id, then_branch))?;
                self.graph.add_edge(Edge::new(block_id, else_branch))?;
            }
            _ => {}
        }
        if let Some(new_terminator) = new_terminator {
            self.block_mut(block_id)?.terminator.replace(new_terminator);
        }
        Ok(())
    }

    pub fn graph(&self) -> &Graph {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph {
        &mut self.graph
    }

    pub fn block_exists(&self, block: NodeId) -> bool {
        self.graph.node_exists(block)
    }

    pub fn block(&self, block: NodeId) -> Result<&BasicBlock<'cfg>> {
        self.blocks.get(&block).ok_or(Error::InvalidBlock(block))
    }

    pub fn block_mut(&mut self, block: NodeId) -> Result<&mut BasicBlock<'cfg>> {
        self.blocks
            .get_mut(&block)
            .ok_or(Error::InvalidBlock(block))
    }

    pub fn blocks(&self) -> &FxHashMap<NodeId, BasicBlock<'cfg>> {
        &self.blocks
    }

    pub fn blocks_mut(&mut self) -> &mut FxHashMap<NodeId, BasicBlock<'cfg>> {
        &mut self.blocks
    }

    pub fn new_block(&mut self) -> Result<NodeId> {
        let node = self.graph.add_node()?;
        self.blocks.insert(node, BasicBlock::default());
        Ok(node)
    }
}
