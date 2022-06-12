use std::collections::binary_heap::Iter;

use graph::{Edge, NodeId};

use super::{
    block::BasicBlock,
    error::{Error, Result},
    function::Function,
    instruction::{
        branch_info::BranchInfo,
        location::{InstructionIndex, InstructionLocation},
        value_info::ValueInfo,
        Inner, Phi, Terminator,
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
            Err(Error::InvalidBlock(block_id))
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
    fn block(&self) -> &BasicBlock {
        self.function.block(self.block).unwrap()
    }

    fn block_mut(&mut self) -> &mut BasicBlock {
        self.function.block_mut(self.block).unwrap()
    }

    fn update_def_use<T: ValueInfo>(&mut self, location: &InstructionLocation, info: &T) {
        self.function.def_use.unregister(location);
        self.function
            .def_use
            .register(location, &info.values_read(), &info.values_written());
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

    pub fn instruction(
        &mut self,
        instruction_index: InstructionIndex,
    ) -> Result<InstructionBuilder> {
        // TODO: check instruction index in bound
        let instruction_location = InstructionLocation(self.block, instruction_index);

        Ok(InstructionBuilder {
            function: self.function,
            instruction_location,
        })
    }

    pub fn instruction_indexes(&self) -> Vec<InstructionIndex> {
        let block = self.block();
        let mut instruction_indices = Vec::with_capacity(
            block.phi_instructions.len()
                + block.inner_instructions.len()
                + if block.terminator.is_some() { 1 } else { 0 },
        );
        instruction_indices
            .extend((0..block.phi_instructions.len()).map(|i| InstructionIndex::Phi(i)));
        instruction_indices
            .extend((0..block.inner_instructions.len()).map(|i| InstructionIndex::Inner(i)));
        if block.terminator.is_some() {
            instruction_indices.push(InstructionIndex::Terminator);
        }
        instruction_indices
    }

    // TODO: remove_instruction function that inserts nops when removing from not the end
    pub(crate) fn clear_phi_instructions(&mut self) {
        for i in 0..self.block().phi_instructions.len() {
            self.function
                .def_use
                .unregister(&InstructionLocation(self.block, InstructionIndex::Inner(i)))
        }
        self.block_mut().phi_instructions.clear();
    }

    pub(crate) fn phi_instructions(&self) -> &Vec<Phi> {
        &self.block().phi_instructions
    }

    /// Push an instruction to the block.
    pub fn push(&mut self, instruction: Inner) -> &mut Self {
        let index = self.block_mut().inner_instructions.len();
        self.function.def_use.register(
            &InstructionLocation(self.block, InstructionIndex::Inner(index)),
            &instruction.values_read(),
            &instruction.values_written(),
        );
        self.block_mut().inner_instructions.push(instruction);
        self
    }

    /// Push a phi instruction to the block.
    pub fn push_phi(&mut self, instruction: Phi) -> &mut Self {
        let index = self.block_mut().inner_instructions.len();
        self.function.def_use.register(
            &InstructionLocation(self.block, InstructionIndex::Phi(index)),
            &instruction.values_read(),
            &instruction.values_written(),
        );
        self.block_mut().phi_instructions.push(instruction);
        self
    }

    pub fn remove(&mut self, instruction_index: InstructionIndex) -> Result<&mut Self> {
        self.function.def_use.unregister(&InstructionLocation(self.block, instruction_index));
        match instruction_index {
            InstructionIndex::Phi(phi_index) => {
                self.block_mut().phi_instructions.remove(phi_index);
                for i in phi_index..self.block_mut().phi_instructions.len() {
                    
                }
            },
            InstructionIndex::Inner(inner_index) => {
                self.block_mut().inner_instructions.remove(inner_index);
                todo!();
            },
            InstructionIndex::Terminator => self.block_mut().terminator = None,
        };
        Ok(self)
    }

    /// Replace the terminator of the block with the given terminator.
    pub fn replace_terminator(&mut self, terminator: Terminator) -> Result<&mut Self> {
        self.update_def_use(
            &InstructionLocation(self.block, InstructionIndex::Terminator),
            &terminator,
        );
        if let Some(old_terminator) = self.block_mut().terminator() {
            for branch in old_terminator.branches().iter().cloned() {
                self.function
                    .graph_mut()
                    .remove_edge(Edge::new(self.block, branch))?;
            }
        }
        for &successor in terminator.branches().iter() {
            self.function
                .graph_mut()
                .add_edge(Edge::new(self.block, successor))?;
        }
        self.block_mut().terminator_mut().replace(terminator);
        Ok(self)
    }
}

pub struct InstructionBuilder<'a> {
    function: &'a mut Function,
    instruction_location: InstructionLocation,
}

impl<'a> InstructionBuilder<'a> {
    pub fn values_read(&self) -> Box<[ValueId]> {
        self.value_info().values_read()
    }

    pub fn values_written(&self) -> Box<[ValueId]> {
        self.value_info().values_written()
    }

    fn value_info(&self) -> &dyn ValueInfo {
        let InstructionLocation(block, instruction_index) = self.instruction_location;
        let block = self.function.block(block).unwrap();
        match instruction_index {
            InstructionIndex::Phi(index) => block.phi_instructions.get(index).unwrap(),
            InstructionIndex::Inner(index) => block.inner_instructions.get(index).unwrap(),
            InstructionIndex::Terminator => block.terminator.as_ref().unwrap(),
        }
    }

    fn value_info_mut(&mut self) -> &mut dyn ValueInfo {
        let InstructionLocation(block, instruction_index) = self.instruction_location;
        let block = self.function.block_mut(block).unwrap();
        match instruction_index {
            InstructionIndex::Phi(index) => block.phi_instructions.get_mut(index).unwrap(),
            InstructionIndex::Inner(index) => block.inner_instructions.get_mut(index).unwrap(),
            InstructionIndex::Terminator => block.terminator.as_mut().unwrap(),
        }
    }

    // TODO: should we have def_use.update?
    pub fn replace_values_read(&mut self, old_value: ValueId, new_value: ValueId) {
        self.function.def_use.unregister(&self.instruction_location);
        let value_info = self.value_info_mut();
        value_info.replace_values_read(old_value, new_value);
        let values_read = value_info.values_read();
        let values_written = value_info.values_written();
        self.function
            .def_use
            .register(&self.instruction_location, &values_read, &values_written);
    }

    pub fn replace_values_written(&mut self, old_value: ValueId, new_value: ValueId) {
        self.function.def_use.unregister(&self.instruction_location);
        let value_info = self.value_info_mut();
        value_info.replace_values_written(old_value, new_value);
        let values_read = value_info.values_read();
        let values_written = value_info.values_written();
        self.function
            .def_use
            .register(&self.instruction_location, &values_read, &values_written);
    }

    pub fn replace_values(&mut self, old_value: ValueId, new_value: ValueId) {
        self.function.def_use.unregister(&self.instruction_location);
        let value_info = self.value_info_mut();
        value_info.replace_values(old_value, new_value);
        let values_read = value_info.values_read();
        let values_written = value_info.values_written();
        self.function
            .def_use
            .register(&self.instruction_location, &values_read, &values_written);
    }

    // TODO: instead of indices, use references?
    pub fn replace_value_read(&mut self, value_index: usize, new_value: ValueId) -> Result<()> {
        self.function.def_use.unregister(&self.instruction_location);
        let value_info = self.value_info_mut();
        let mut values_read = value_info.values_read_mut();
        let value = values_read
            .get_mut(value_index)
            .ok_or(Error::IndexOutOfBounds(value_index))?;
        **value = new_value;
        let values_read = value_info.values_read();
        let values_written = value_info.values_written();
        self.function
            .def_use
            .register(&self.instruction_location, &values_read, &values_written);

        Ok(())
    }

    pub fn replace_value_written(&mut self, value_index: usize, new_value: ValueId) -> Result<()> {
        self.function.def_use.unregister(&self.instruction_location);
        let value_info = self.value_info_mut();
        let mut values_written = value_info.values_written_mut();
        let value = values_written
            .get_mut(value_index)
            .ok_or(Error::IndexOutOfBounds(value_index))?;
        **value = new_value;
        let values_read = value_info.values_read();
        let values_written = value_info.values_written();
        self.function
            .def_use
            .register(&self.instruction_location, &values_read, &values_written);

        Ok(())
    }
}
