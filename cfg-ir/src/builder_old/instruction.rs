use crate::{
    error::{Error, Result},
    function::Function,
    instruction::location::{InstructionLocation, InstructionIdx},
    value::{ValueId, ValueInfo},
};

pub struct InstructionBuilder<'a> {
    function: &'a mut Function,
    instruction_location: InstructionLocation,
}

impl<'a> InstructionBuilder<'a> {
    fn value_info(&self) -> Result<&dyn ValueInfo> {
        let block = self.function.block(self.instruction_location.0)?;
        let value_info: &dyn ValueInfo = match self.instruction_location.1 {
            InstructionIdx::Terminator => block
                .terminator()
                .as_ref()
                .ok_or_else(|| Error::NoTerminator)?,
            InstructionIdx::Body(instruction) => block
                .instructions
                .get(instruction)
                .ok_or_else(|| Error::InvalidBlock {
                    block: self.instruction_location.0,
                })?,
        };
        Ok(value_info)
    }

    fn value_info_mut(&self) -> Result<&mut dyn ValueInfo> {
        let block = self.function.block(self.instruction_location.0)?;
        let value_info: &mut dyn ValueInfo = match self.instruction_location.1 {
            InstructionIdx::Terminator => block
                .terminator()
                .as_mut()
                .ok_or_else(|| Error::NoTerminator)?,
            InstructionIdx::Body(instruction) => block
                .instructions
                .get_mut(instruction)
                .ok_or_else(|| Error::InvalidBlock {
                    block: self.instruction_location.0,
                })?,
        };
        Ok(value_info)
    }

    pub(super) fn new(
        function: &'a mut Function,
        instruction_location: InstructionLocation,
    ) -> Result<Self> {
        Ok(Self {
            function,
            instruction_location,
        })
    }

    pub fn values_read(&self) -> Result<Box<[ValueId]>> {
        Ok(self.value_info()?.values_read())
    }

    pub fn values_written(&self) -> Result<Box<[ValueId]>> {
        Ok(self.value_info()?.values_written())
    }

    pub fn replace_values_read(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        if self
            .function
            .def_use
            .get_mut(old_value)
            .unwrap()
            .reads
            .remove(&self.instruction_location)
        {
            self.function
                .def_use
                .get_mut(new_value)
                .unwrap()
                .reads
                .insert(self.instruction_location);
        }

        self.value_info_mut()?
            .replace_values_read(old_value, new_value);
        Ok(())
    }

    pub fn replace_values_written(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        if self
            .function
            .def_use
            .get_mut(old_value)
            .unwrap()
            .writes
            .remove(&self.instruction_location)
        {
            self.function
                .def_use
                .get_mut(new_value)
                .unwrap()
                .writes
                .insert(self.instruction_location);
        }

        self.value_info_mut()?
            .replace_values_written(old_value, new_value);
        Ok(())
    }

    pub fn replace_values(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        self.replace_values_read(old_value, new_value)?;
        self.replace_values_written(old_value, new_value)
    }

    pub fn set_read_value(&mut self, read_value_index: usize, new_value: ValueId) -> Result<()> {
        let mut instruction_ref = self.value_info_mut()?;
        let mut values_read = instruction_ref.values_read_mut();
        let value_read = values_read
            .get_mut(read_value_index)
            .ok_or(Error::IndexOutOfBounds)?;
        let old_value = **value_read;
        **value_read = new_value;

        if self
            .function
            .def_use
            .get_mut(old_value)
            .unwrap()
            .reads
            .remove(&self.instruction_location)
        {
            self.function
                .def_use
                .get_mut(new_value)
                .unwrap()
                .reads
                .insert(self.instruction_location);
        }

        Ok(())
    }

    pub fn set_written_value(
        &mut self,
        written_value_index: usize,
        new_value: ValueId,
    ) -> Result<()> {
        let mut instruction_ref = self.value_info_mut()?;
        let mut values_written = instruction_ref.values_written_mut();
        let value_written = values_written
            .get_mut(written_value_index)
            .ok_or(Error::IndexOutOfBounds)?;
        let old_value = **value_written;
        **value_written = new_value;

        if self
            .function
            .def_use
            .get_mut(old_value)
            .unwrap()
            .writes
            .remove(&self.instruction_location)
        {
            self.function
                .def_use
                .get_mut(new_value)
                .unwrap()
                .writes
                .insert(self.instruction_location);
        }

        Ok(())
    }
}
