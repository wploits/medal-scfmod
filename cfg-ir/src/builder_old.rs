mod block;
mod instruction;
mod instruction_range;

use graph::NodeId;

pub use block::BlockBuilder;
pub use instruction::InstructionBuilder;
pub use instruction_range::InstructionRange;

use crate::{
    error::{Error, Result},
    function::Function,
    instruction::location::{InstructionLocation, InstructionIdx},
    value::ValueId,
};

pub struct Builder<'a> {
    function: &'a mut Function,
}

impl<'a> Builder<'a> {
    pub fn new(function: &'a mut Function) -> Self {
        Self { function }
    }

    pub fn new_value(&mut self) -> ValueId {
        self.function.new_value()
    }

    pub fn new_block(&mut self) -> Result<BlockBuilder> {
        let block = self.function.add_block()?;
        BlockBuilder::new(self.function, block)
    }

    pub fn new_entry_block(&mut self) -> Result<BlockBuilder> {
        let mut entry_block = self.new_block()?;
        let index = entry_block.index();
        entry_block.mark_as_entry()?;
        self.block(index)
    }

    pub fn block(&mut self, block: NodeId) -> Result<BlockBuilder> {
        if !self.function.block_exists(block) {
            Err(Error::InvalidBlock { block }.into())
        } else {
            BlockBuilder::new(self.function, block)
        }
    }

    pub fn instruction(
        &mut self,
        instruction_location: InstructionLocation,
    ) -> Result<InstructionBuilder> {
        let block = self.function.block(instruction_location.0)?;
        let instruction_exists = match instruction_location.1 {
            InstructionIdx::Terminator => block.terminator().is_some(),
            InstructionIdx::Body(instruction) => {
                block.instructions.get(instruction).is_some()
            }
        };
        if instruction_exists {
            InstructionBuilder::new(self.function, instruction_location)
        } else {
            Err(Error::InvalidInstruction {
                instruction_location,
            })
        }
    }
}
