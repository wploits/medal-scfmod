use graph::{Edge, NodeId};

use super::{
    block::BasicBlock,
    error::{Error, Result},
    function::Function,
    instruction::{
        branch_info::BranchInfo,
        location::{InstructionIdx, InstructionLocation},
        value_info::ValueInfo,
        Instruction, Terminator,
    },
    value::ValueId,
};

pub struct Builder<'a> {
    function: &'a mut Function,
}

impl<'a> Builder<'a> {
    /// Returns a builder for an existing basic block.
    pub fn block(&mut self, block_id: NodeId) -> Result<BlockBuilder> {
        if !self.function.block_exists(block_id) {
            Err(Error::InvalidBlock { block: block_id })
        } else {
            Ok(BlockBuilder {
                function: self.function,
                block: block_id,
            })
        }
    }

    /// Returns a builder for a new basic block.
    pub fn new_block(&mut self) -> Result<BlockBuilder> {
        let block_id = self.function.add_block()?;
        Ok(BlockBuilder {
            function: self.function,
            block: block_id,
        })
    }

    /// Returns a new value.
    pub fn new_value(&mut self) -> ValueId {
        self.function.new_value()
    }

    /// Returns a builder for a function.
    pub fn new(function: &'a mut Function) -> Self {
        Self { function }
    }
}

pub struct BlockBuilder<'a> {
    function: &'a mut Function,
    block: NodeId,
}

impl<'a> BlockBuilder<'a> {
    // TODO: find a way to store a reference to the block in Self
    fn block(&mut self) -> &mut BasicBlock {
        self.function.block_mut(self.block).unwrap()
    }

    fn update_def_use<T: ValueInfo>(&mut self, location: &InstructionLocation, info: &T) {
        self.function.def_use.unregister(location);
        self.function.def_use.register(location, info);
    }

    /// Return an identifier for the block.
    pub fn block_id(&self) -> NodeId {
        self.block
    }

    /// Mark the block as the entry block.
    pub fn mark_entry(&mut self) -> &mut Self {
        self.function.graph_mut().set_entry(self.block).unwrap();
        self
    }

    /// Push an instruction to the block.
    pub fn push(&mut self, instruction: Instruction) -> &mut Self {
        let index = self.block().instructions.len();
        self.function.def_use.register(
            &InstructionLocation(self.block, InstructionIdx::Body(index)),
            &instruction,
        );
        self.block().instructions.push(instruction);
        self
    }

    /// Replace the terminator of the block with the given terminator.
    pub fn replace_terminator(&mut self, terminator: Terminator) -> Result<&mut Self> {
        self.update_def_use(
            &InstructionLocation(self.block, InstructionIdx::Terminator),
            &terminator,
        );
        if let Some(old_terminator) = self.block().terminator() {
            for branch in old_terminator.branches().iter().cloned() {
                self.function
                    .graph_mut()
                    .remove_edge(Edge(self.block, branch))?;
            }
        }
        for &successor in terminator.branches().iter() {
            self.function
                .graph_mut()
                .add_edge(Edge::new(self.block, successor))?;
        }
        self.block().terminator_mut().replace(terminator);
        Ok(self)
    }
}
