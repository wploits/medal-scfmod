use super::instruction::{Instruction, Phi, Terminator};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub(crate) phis: Vec<Phi>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) terminator: Option<Terminator>,
}

impl BasicBlock {
    pub(crate) fn new() -> Self {
        Self {
            phis: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }

    pub(crate) fn terminator_mut(&mut self) -> &mut Option<Terminator> {
        &mut self.terminator
    }
}
