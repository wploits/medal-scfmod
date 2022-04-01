use graph::NodeId;
use std::ops::Range;

use crate::{
    error::{Error, Result},
    function::Function,
    instruction::location::{InstructionLocation, InstructionIdx},
    value::ValueId,
};

use super::InstructionBuilder;

pub struct InstructionRange<'a> {
    function: &'a mut Function,
    block: NodeId,
    instruction_location_range: Range<InstructionIdx>,
}

impl<'a> InstructionRange<'a> {
    pub(super) fn new(
        function: &'a mut Function,
        block: NodeId,
        instruction_location_range: Range<InstructionIdx>,
    ) -> Result<Self> {
        if instruction_location_range.start > instruction_location_range.end {
            Err(Error::InvalidInstructionRange {
                block,
                instruction_location_range,
            })
        } else {
            Ok(Self {
                function,
                block,
                instruction_location_range,
            })
        }
    }

    pub fn replace_values_read(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        let mut local_location_start = self.instruction_location_range.start;

        loop {
            let mut instruction = InstructionBuilder::new(
                self.function,
                InstructionLocation(self.block, local_location_start),
            )?;
            instruction.replace_values_read(old_value, new_value)?;

            if local_location_start == self.instruction_location_range.end {
                break;
            }

            let block = self.function.block(self.block)?;
            // TODO: move to LocalInstructionInfo::next(block)? (see also: BlockBuilder::next_instruction_location)
            match &mut local_location_start {
                InstructionIdx::Body(instruction_index) => {
                    if *instruction_index == block.instructions.len() - 1 {
                        local_location_start = InstructionIdx::Terminator;
                    } else {
                        *instruction_index += 1;
                    }
                }
                InstructionIdx::Terminator {} => {
                    return Err(Error::InvalidInstruction {
                        instruction_location: InstructionLocation(
                            self.block,
                            self.instruction_location_range.end,
                        ),
                    });
                }
            }
        }

        Ok(())
    }

    pub fn replace_values_written(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        todo!();
    }
}
