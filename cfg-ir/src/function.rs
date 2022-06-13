use graph::{Edge, Graph, NodeId};
use std::collections::HashMap;

use crate::{
    block::BasicBlock,
    error::{Error, Result},
    instruction::{branch_info::BranchInfo, Terminator},
    value::ValueId,
};

#[derive(Debug, Clone)]
pub struct Function {
    graph: Graph,
    blocks: HashMap<NodeId, BasicBlock>,
    next_value_index: usize,
    entry: Option<NodeId>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            blocks: HashMap::new(),
            next_value_index: 0,
            entry: None,
        }
    }

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
        if let Some(new_terminator) = new_terminator {
            for successor in new_terminator.branches() {
                self.graph.add_edge(Edge::new(block_id, successor))?;
            }
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

    pub fn block(&self, block: NodeId) -> Result<&BasicBlock> {
        self.blocks.get(&block).ok_or(Error::InvalidBlock(block))
    }

    pub fn block_mut(&mut self, block: NodeId) -> Result<&mut BasicBlock> {
        self.blocks
            .get_mut(&block)
            .ok_or(Error::InvalidBlock(block))
    }

    pub fn blocks(&self) -> &HashMap<NodeId, BasicBlock> {
        &self.blocks
    }

    pub fn blocks_mut(&mut self) -> &mut HashMap<NodeId, BasicBlock> {
        &mut self.blocks
    }

    pub fn new_block(&mut self) -> Result<NodeId> {
        let node = self.graph.add_node()?;
        self.blocks.insert(node, BasicBlock::new());
        Ok(node)
    }

    pub fn new_value(&mut self) -> ValueId {
        let value = self.next_value_index;
        self.next_value_index += 1;
        ValueId(value)
    }

    pub fn values(&self) -> Vec<ValueId> {
        (0..self.next_value_index).map(|v| ValueId(v)).collect()
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}
