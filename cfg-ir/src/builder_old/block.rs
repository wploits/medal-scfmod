use std::cmp::Ordering;
use graph::{NodeId, edge::Edge};

use crate::{
    constant::Constant,
    error::{Error, Result},
    function::Function,
    instruction::{
        location::{InstructionLocation, InstructionIdx},
        Binary, BinaryOp, Call, Concat, ConditionalJump, Instruction, LoadConstant,
        LoadGlobal, LoadIndex, Move, Phi, Return, StoreGlobal, StoreIndex, Terminator,
        Unary, UnaryOp, UnconditionalJump,
    },
    value::{ValueId, ValueInfo},
};

use super::{InstructionBuilder, InstructionRange};

pub struct BlockBuilder<'a> {
    function: &'a mut Function,
    block: NodeId,
    cursor_index: usize,
}

impl<'a> BlockBuilder<'a> {
    pub(super) fn new(function: &'a mut Function, block: NodeId) -> Result<Self> {
        let cursor_index = function.block(block)?.instructions.len();
        Ok(Self {
            function,
            block,
            cursor_index,
        })
    }

    pub fn mark_as_entry(&mut self) -> Result<&mut Self> {
        self.function.graph_mut().set_entry(self.block)?;
        Ok(self)
    }

    pub fn instruction(
        &mut self,
        instruction_location: InstructionIdx,
    ) -> Result<InstructionBuilder> {
        InstructionBuilder::new(
            self.function,
            instruction_location.instruction_location(self.block),
        )
    }

    pub fn instruction_count(&self) -> Result<usize> {
        Ok(self
            .function
            .block(self.block)?
            .instructions
            .len())
    }

    pub fn phi_count(&self) -> Result<usize> {
        Ok(self.function.block(self.block)?.phis.len())
    }

    pub fn instruction_locations(&mut self) -> Result<Box<[InstructionIdx]>> {
        let block_index = self.block;
        let phi_count = self.phi_count()?;
        let instruction_count = self.instruction_count()?;
        let has_terminator = self.function.terminator(block_index)?.is_some();
        let mut v =
            Vec::with_capacity(phi_count + instruction_count + if has_terminator { 1 } else { 0 });

        for phi_index in 0..phi_count {
            v.push(InstructionIdx::Phi(PhiInstructionIdx {
                phi_index,
            }));
        }

        for instruction_index in 0..instruction_count {
            v.push(InstructionIdx::Body(
                BodyInstructionIdx { instruction_index },
            ));
        }

        if has_terminator {
            v.push(InstructionIdx::Terminator {});
        }

        Ok(v.into_boxed_slice())
    }

    pub fn next_instruction_location(
        &self,
        mut instruction_location: InstructionIdx,
    ) -> Result<InstructionIdx> {
        let block = self.function.block(self.block)?;
        // TODO: move to LocalInstructionInfo::next(block)? (see also: InstructionRange::replace_values_read and InstructionRange::replace_values_written)
        match &mut instruction_location {
            InstructionIdx::Phi(phi_instruction_location) => {
                if phi_instruction_location.phi_index == block.phis.len() - 1 {
                    instruction_location =
                        InstructionIdx::Body(BodyInstructionIdx {
                            instruction_index: 0,
                        });
                } else {
                    phi_instruction_location.phi_index += 1;
                }
            }
            InstructionIdx::Body(body_instruction_location) => {
                if body_instruction_location.instruction_index == block.instructions.len() - 1 {
                    instruction_location = InstructionIdx::Terminator {};
                } else {
                    body_instruction_location.instruction_index += 1;
                }
            }
            InstructionIdx::Terminator {} => {
                return Err(FunctionError::InvalidInstruction {
                    instruction_location: instruction_location
                        .instruction_location(self.block),
                }
                .into());
            }
        }

        Ok(instruction_location)
    }

    pub fn instruction_range(
        &mut self,
        start: InstructionIdx,
        end: Option<InstructionIdx>,
    ) -> Result<InstructionRange> {
        let end = end.unwrap_or(InstructionIdx::Terminator {});
        InstructionRange::new(self.function, self.block, start..end)
    }

    pub fn replace_values_read(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        for &instruction_location in self.instruction_locations()?.iter() {
            let mut replace_instr = self.instruction(instruction_location)?;
            replace_instr.replace_values_read(old_value, new_value)?;
        }

        Ok(())
    }

    pub fn replace_values_written(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        for &instruction_location in self.instruction_locations()?.iter() {
            let mut replace_instr = self.instruction(instruction_location)?;
            replace_instr.replace_values_written(old_value, new_value)?;
        }

        Ok(())
    }

    pub fn replace_values(&mut self, old_value: ValueId, new_value: ValueId) -> Result<()> {
        self.replace_values_read(old_value, new_value)?;
        self.replace_values_written(old_value, new_value)
    }

    pub fn set_cursor(&mut self, new_cursor_index: usize) -> Result<&mut Self> {
        if new_cursor_index
            <= self
                .function
                .block(self.block)?
                .instructions
                .len()
        {
            self.cursor_index = new_cursor_index;
            Ok(self)
        } else {
            Err(Error::IndexOutOfBounds.into())
        }
    }

    pub fn cursor(&self) -> usize {
        self.cursor_index
    }

    pub fn index(&self) -> usize {
        self.block
    }

    // TODO: if you're gonna uncomment this, make sure you adjust function.values
    // pub(crate) fn split_block_at(&mut self, split_at: usize) -> Result<usize> {
    //     if let Some(current_block) = self.selected_block {
    //         let new_block = self.new_block()?;
    //         for predecessor in self.function.graph.predecessors[&current_block].clone() {
    //             self.select_block(predecessor)?;
    //             self.replace_branch(current_block, new_block)?;
    //         }
    //         for _ in 0..self.function.block(current_block)?.instructions.len() - split_at {
    //             let instruction = self.function.block_mut(current_block)?.instructions.remove(0);
    //             self.function
    //                 .block_mut(new_block)?
    //                 .instructions
    //                 .push(instruction);
    //         }
    //         self.select_block(new_block)?;
    //         self.set_cursor(self.instruction_len()?)?;
    //         self.seal_with_unconditional_jump(current_block)?;
    //         Ok(new_block)
    //     } else {
    //         Err(Error::BuilderError(Error::NoBlockSelected))
    //     }
    // }

    pub fn remove_phi(&mut self, phi_index: usize) -> Result<&mut Self> {
        let block = self.function.block_mut(self.block)?;

        if !block.phis.is_empty() {
            // TODO: insert nop instead and measure performance difference
            block.phis.remove(phi_index);

            for def_use_info in self.function.def_use.values_mut() {
                for read in def_use_info.reads.iter().cloned().collect::<Vec<_>>() {
                    if let InstructionLocation::Phi(phi_instruction_location) = read {
                        if phi_instruction_location.block_index() == self.block {
                            match phi_instruction_location.phi_index.cmp(&phi_index) {
                                Ordering::Equal => {
                                    def_use_info.reads.remove(&read);
                                }
                                Ordering::Greater => {
                                    def_use_info.reads.remove(&read);
                                    def_use_info.reads.insert(InstructionLocation::Phi(
                                        PhiInstructionLocation {
                                            block_index: phi_instruction_location.block_index,
                                            phi_index: phi_instruction_location.phi_index + 1,
                                        },
                                    ));
                                }
                                _ => {}
                            }
                        }
                    }
                }

                for write in def_use_info.writes.iter().cloned().collect::<Vec<_>>() {
                    if let InstructionLocation::Phi(phi_instruction_location) = write {
                        if phi_instruction_location.block_index() == self.block {
                            match phi_instruction_location.phi_index.cmp(&phi_index) {
                                Ordering::Equal => {
                                    def_use_info.writes.remove(&write);
                                }
                                Ordering::Greater => {
                                    def_use_info.writes.remove(&write);
                                    def_use_info.writes.insert(InstructionLocation::Phi(
                                        PhiInstructionLocation {
                                            block_index: phi_instruction_location.block_index,
                                            phi_index: phi_instruction_location.phi_index + 1,
                                        },
                                    ));
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            Ok(self)
        } else {
            Err(Error::NoInstructions.into())
        }
    }

    pub fn clear_phis(&mut self) -> Result<&mut Self> {
        for phi_index in (0..self.phi_count()?).rev() {
            self.remove_phi(phi_index)?;
        }

        Ok(self)
    }

    pub fn remove_instruction(&mut self, instruction_index: usize) -> Result<&mut Self> {
        let block = self.function.block_mut(self.block)?;

        if !block.instructions.is_empty() {
            // TODO: insert nop instead and measure performance difference
            block.instructions.remove(instruction_index);

            for def_use_info in self.function.def_use.values_mut() {
                for read in def_use_info.reads.iter().cloned().collect::<Vec<_>>() {
                    if let InstructionIdx::Body(body_instruction_location) = read.1 {
                        if read.0 == self.block {
                            match body_instruction_location
                                .cmp(&instruction_index)
                            {
                                Ordering::Equal => {
                                    def_use_info.reads.remove(&read);
                                }
                                Ordering::Greater => {
                                    def_use_info.reads.remove(&read);
                                    def_use_info.reads.insert(InstructionLocation(self.block, InstructionIdx::Body(body_instruction_location + 1)));
                                }
                                _ => {}
                            }
                        }
                    }
                }

                for write in def_use_info.writes.iter().cloned().collect::<Vec<_>>() {
                    if let InstructionIdx::Body(body_instruction_location) = write.1 {
                        if write.0 == self.block {
                            match body_instruction_location
                                .cmp(&instruction_index)
                            {
                                Ordering::Equal => {
                                    def_use_info.writes.remove(&write);
                                }
                                Ordering::Greater => {
                                    def_use_info.writes.remove(&write);
                                    def_use_info.writes.insert(
                                        InstructionLocation(self.block, InstructionIdx::Body(body_instruction_location + 1))
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            Ok(self)
        } else {
            Err(Error::NoInstructions.into())
        }
    }

    pub(crate) fn insert_phi(&mut self, phi_index: usize, phi: Phi) -> Result<&mut Self> {
        let block = self.function.block_mut(self.block)?;

        // TODO: readjust def_use for this block's phis
        if phi_index != block.phis.len() {
            todo!();
        }

        block.phis.insert(phi_index, phi);

        let phi = &block.phis[phi_index];

        for &value in phi.values_written().iter() {
            self.function
                .def_use
                .get_mut(value)
                .unwrap()
                .writes
                .insert(InstructionLocation(self.block, InstructionIdx::Body(phi_index)));
        }

        let phi = &self.function.block(self.block)?.phis[phi_index];
        for &value in phi.values_read().iter() {
            self.function
                .def_use
                .get_mut(value)
                .unwrap()
                .reads
                .insert(InstructionLocation(self.block, InstructionIdx::Body(phi_index)));
        }

        Ok(self)
    }

    pub(crate) fn insert_instruction(
        &mut self,
        instruction_index: usize,
        instruction: Instruction,
    ) -> Result<&mut Self> {
        let block = self.function.block_mut(self.block)?;

        // TODO: readjust def_use for this block
        if instruction_index != block.instructions.len() {
            todo!();
        }

        block.instructions.insert(instruction_index, instruction);

        let instruction = &block.instructions[instruction_index];

        for &value in instruction.values_written().iter() {
            self.function
                .def_use
                .get_mut(value)
                .unwrap()
                .writes
                .insert(InstructionLocation(self.block, InstructionIdx::Body(instruction_index)));
        }

        let instruction =
            &self.function.block(self.block)?.instructions[instruction_index];
        for &value in instruction.values_read().iter() {
            self.function
                .def_use
                .get_mut(value)
                .unwrap()
                .reads
                .insert(InstructionLocation(self.block, InstructionIdx::Body(instruction_index)));
        }

        Ok(self)
    }

    fn push_instruction(&mut self, instruction: Instruction) -> Result<&mut Self> {
        let terminator = self.function.block(self.block)?.terminator();
        if terminator.is_some() {
            return Err(Error::BlockSealed.into());
        }

        let old_cursor_index = self.cursor_index;
        self.cursor_index += 1;

        self.insert_instruction(old_cursor_index, instruction)
    }

    pub fn seal_with(&mut self, new_terminator: Terminator) -> Result<&mut Self> {
        let block = self.function.block_mut(self.block)?;

        let terminator = block.terminator_mut();
        if terminator.is_some() {
            return Err(Error::BlockSealed.into());
        }

        terminator.replace(new_terminator);

        for &value in terminator.as_ref().unwrap().values_written().iter() {
            self.function
                .def_use
                .get_mut(value)
                .unwrap()
                .writes
                .insert(InstructionLocation(self.block, InstructionIdx::Terminator));
        }

        for &value in terminator.as_ref().unwrap().values_read().iter() {
            self.function.def_use.get_mut(value).unwrap().reads.insert(
                InstructionLocation(self.block, InstructionIdx::Terminator)
            );
        }

        Ok(self)
    }

    pub fn unseal(&mut self) -> Result<Terminator> {
        let successors = self.function.graph().successors(self.block).cloned().collect::<Vec<_>>();
        if !successors.is_empty() {
            // TODO: find another way to do it, because it's stupid
            for &successor in successors.iter() {
                self.function.graph_mut().remove_edge(Edge(self.block, successor));
            }
        }

        if let Some(terminator) = self
            .function
            .block_mut(self.block)?
            .terminator_mut()
            .take()
        {
            let instruction_location =
                InstructionLocation(self.block, InstructionIdx::Terminator);
            for &value in terminator.values_written().iter() {
                self.function
                    .def_use
                    .get_mut(value)
                    .unwrap()
                    .writes
                    .remove(&instruction_location);
            }

            for &value in terminator.values_read().iter() {
                self.function
                    .def_use
                    .get_mut(value)
                    .unwrap()
                    .reads
                    .remove(&instruction_location);
            }

            Ok(terminator)
        } else {
            Err(Error::NoTerminator.into())
        }
    }

    pub fn replace_branch(&mut self, old: usize, new: usize) -> Result<&mut Self> {
        self.function
            .block_mut(self.block)?
            .terminator_mut()
            .as_mut()
            .ok_or(Error::NoTerminator)?
            .replace_branch(old, new);
        self.function.graph_mut().remove_edge(Edge(self.block, old))?;
        self.function.graph_mut().add_edge(Edge(self.block, new))?;
        Ok(self)
    }

    // TODO: insert_load_constant, etc.
    pub fn load_constant(&mut self, dest: ValueId, value: Constant) -> Result<&mut Self> {
        self.push_instruction(Instruction::LoadConstant(LoadConstant {
            dest,
            constant: value,
        }))
    }

    pub fn load_global(&mut self, dest: ValueId, value: String) -> Result<&mut Self> {
        self.push_instruction(Instruction::LoadGlobal(LoadGlobal { dest, name: value }))
    }

    pub fn load_index(
        &mut self,
        dest: ValueId,
        object: ValueId,
        key: ValueId,
    ) -> Result<&mut Self> {
        self.push_instruction(Instruction::LoadIndex(LoadIndex { dest, object, key }))
    }

    pub fn load_value(&mut self, dest: ValueId, source: ValueId) -> Result<&mut Self> {
        self.push_instruction(Instruction::Move(Move { dest, source }))
    }

    pub fn store_global(&mut self, value: ValueId, dest: String) -> Result<&mut Self> {
        self.push_instruction(Instruction::StoreGlobal(StoreGlobal { value, dest }))
    }

    pub fn store_index(
        &mut self,
        object: ValueId,
        key: ValueId,
        value: ValueId,
    ) -> Result<&mut Self> {
        self.push_instruction(Instruction::StoreIndex(StoreIndex { value, object, key }))
    }

    pub fn binary(
        &mut self,
        dest: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: BinaryOp,
    ) -> Result<&mut Self> {
        self.push_instruction(Instruction::Binary(Binary { dest, lhs, rhs, op }))
    }

    pub fn unary(&mut self, dest: ValueId, value: ValueId, op: UnaryOp) -> Result<&mut Self> {
        self.push_instruction(Instruction::Unary(Unary { dest, value, op }))
    }

    pub fn concat(&mut self, dest: ValueId, values: Vec<ValueId>) -> Result<&mut Self> {
        self.push_instruction(Instruction::Concat(Concat { dest, values }))
    }

    pub fn call(
        &mut self,
        function: ValueId,
        arguments: Vec<ValueId>,
        return_values: Vec<ValueId>,
        variadic_arguments: bool,
        variadic_return: bool,
    ) -> Result<&mut Self> {
        self.push_instruction(Instruction::Call(Call {
            function,
            arguments,
            variadic_arguments,
            return_values,
            variadic_return,
        }))
    }

    // TODO: terminators
    pub fn seal_with_conditional_jump(
        &mut self,
        condition: ValueId,
        true_branch: usize,
        false_branch: usize,
    ) -> Result<&mut Self> {
        self.function
            .graph_mut()
            .add_edge(Edge(self.block, true_branch))?;
        self.function
            .graph_mut()
            .add_edge(Edge(self.block, false_branch))?;

        self.seal_with(Terminator::ConditionalJump(ConditionalJump {
            condition,
            true_branch,
            false_branch,
        }))
    }

    pub fn seal_with_unconditional_jump(&mut self, branch: usize) -> Result<&mut Self> {
        self.function.graph().add_edge(Edge(self.block, branch))?;
        self.seal_with(Terminator::UnconditionalJump(UnconditionalJump { branch }))
    }

    pub fn seal_with_ret(&mut self, values: Vec<ValueId>, variadic: bool) -> Result<&mut Self> {
        self.seal_with(Terminator::Return(Return { values, variadic }))
    }
}
