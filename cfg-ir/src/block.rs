use super::instruction::{Inner, Phi, Terminator};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub(crate) phi_instructions: Vec<Phi>,
    pub(crate) inner_instructions: Vec<Inner>,
    pub(crate) terminator: Option<Terminator>,
}

impl BasicBlock {
    pub(crate) fn new() -> Self {
        Self {
            phi_instructions: Vec::new(),
            inner_instructions: Vec::new(),
            terminator: None,
        }
    }

    pub fn instructions(&self) -> impl Iterator<Item = &Inner> + '_ {
        self.inner_instructions.iter()
    }

    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }

    pub(crate) fn terminator_mut(&mut self) -> &mut Option<Terminator> {
        &mut self.terminator
    }
}
