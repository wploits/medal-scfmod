use crate::instruction::value_info::ValueInfo;

use super::instruction::{location::InstructionIndex, Inner, Phi, Terminator};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub phi_instructions: Vec<Phi>,
    pub inner_instructions: Vec<Inner>,
    pub(crate) terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            phi_instructions: Vec::new(),
            inner_instructions: Vec::new(),
            terminator: None,
        }
    }

    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }

    pub fn indices(&self) -> Vec<InstructionIndex> {
        let phi_indices = self
            .phi_instructions
            .iter()
            .enumerate()
            .map(|(i, _)| InstructionIndex::Phi(i));
        let inner_indices = self
            .inner_instructions
            .iter()
            .enumerate()
            .map(|(i, _)| InstructionIndex::Inner(i));
        let terminator_index = self.terminator.iter().map(|_| InstructionIndex::Terminator);
        phi_indices
            .chain(inner_indices)
            .chain(terminator_index)
            .collect()
    }

    pub(crate) fn value_info(&self, index: InstructionIndex) -> Option<&dyn ValueInfo> {
        match index {
            InstructionIndex::Phi(i) => self.phi_instructions.get(i).map(|phi| phi as _),
            InstructionIndex::Inner(i) => self.inner_instructions.get(i).map(|inner| inner as _),
            InstructionIndex::Terminator => {
                self.terminator.as_ref().map(|terminator| terminator as _)
            }
        }
    }

    pub(crate) fn value_info_mut(&mut self, index: InstructionIndex) -> Option<&mut dyn ValueInfo> {
        match index {
            InstructionIndex::Phi(i) => self.phi_instructions.get_mut(i).map(|phi| phi as _),
            InstructionIndex::Inner(i) => {
                self.inner_instructions.get_mut(i).map(|inner| inner as _)
            }
            InstructionIndex::Terminator => {
                self.terminator.as_mut().map(|terminator| terminator as _)
            }
        }
    }
}
