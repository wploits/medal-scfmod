use graph::{Graph, NodeId};
use std::collections::HashMap;

use crate::{
    block::BasicBlock,
    def_use::DefUse,
    error::{Error, Result},
    value::ValueId,
};

#[derive(Debug, Clone)]
pub struct Function {
    graph: Graph,
    blocks: HashMap<NodeId, BasicBlock>,
    next_value_index: ValueId,
    pub(crate) def_use: DefUse,
}

impl Function {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            blocks: HashMap::new(),
            next_value_index: 0,
            def_use: DefUse::new(),
        }
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
        self.blocks.get(&block).ok_or(Error::InvalidBlock { block })
    }

    pub(crate) fn block_mut(&mut self, block: NodeId) -> Result<&mut BasicBlock> {
        self.blocks
            .get_mut(&block)
            .ok_or(Error::InvalidBlock { block })
    }

    pub fn add_block(&mut self) -> Result<NodeId> {
        let node = self.graph.add_node()?;
        self.blocks.insert(node, BasicBlock::new());
        Ok(node)
    }

    pub fn new_value(&mut self) -> ValueId {
        let value = self.next_value_index;
        self.next_value_index += 1;
        value
    }

    pub(crate) fn value_count(&self) -> usize {
        self.next_value_index
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new()
    }
}
