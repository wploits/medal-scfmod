use graph::NodeId;

#[derive(Debug, Clone)]
pub enum Terminator {
    Jump(NodeId),
    Conditional(NodeId, NodeId),
    Return,
}

#[derive(Debug, Clone)]
pub struct BasicBlock<'a> {
    // pub phi_instructions: Vec<Phi>,
    pub block: ast::Block<'a>,
    pub terminator: Option<Terminator>,
}

impl Default for BasicBlock<'_> {
    fn default() -> Self {
        Self {
            block: ast::Block::new(),
            terminator: None,
        }
    }
}

impl<'a> BasicBlock<'a> {
    pub fn terminator(&self) -> &Option<Terminator> {
        &self.terminator
    }

    /*pub fn indices(&self) -> Vec<InstructionIndex> {
        let phi_indices = self
            .phi_instructions
            .iter()
            .enumerate()
            .map(|(i, _)| InstructionIndex::Phi(i));
        let inner_indices = self
            .statements
            .iter()
            .enumerate()
            .map(|(i, _)| InstructionIndex::Inner(i));
        let terminator_index = self.terminator.iter().map(|_| InstructionIndex::Terminator);
        phi_indices
            .chain(inner_indices)
            .chain(terminator_index)
            .collect()
    }

    pub fn values_written(&self, index: InstructionIndex) -> Vec<ValueId> {
        match index {
            InstructionIndex::Phi(i) => self.phi_instructions.get(i).unwrap().values_written(),
            InstructionIndex::Inner(i) => self.statements.get(i).unwrap().values_written(),
            InstructionIndex::Terminator => self.terminator.as_ref().unwrap().values_written(),
        }
    }

    pub fn values_written_mut(&mut self, index: InstructionIndex) -> Vec<&mut ValueId> {
        match index {
            InstructionIndex::Phi(i) => self
                .phi_instructions
                .get_mut(i)
                .unwrap()
                .values_written_mut(),
            InstructionIndex::Inner(i) => self
                .inner_instructions
                .get_mut(i)
                .unwrap()
                .values_written_mut(),
            InstructionIndex::Terminator => self.terminator.as_mut().unwrap().values_written_mut(),
        }
    }

    pub fn values_read(&self, index: InstructionIndex) -> Vec<ValueId> {
        match index {
            InstructionIndex::Phi(i) => self.phi_instructions.get(i).unwrap().values_read(),
            InstructionIndex::Inner(i) => self.inner_instructions.get(i).unwrap().values_read(),
            InstructionIndex::Terminator => self.terminator.as_ref().unwrap().values_read(),
        }
    }

    pub fn values_read_mut(&mut self, index: InstructionIndex) -> Vec<&mut ValueId> {
        match index {
            InstructionIndex::Phi(i) => self.phi_instructions.get_mut(i).unwrap().values_read_mut(),
            InstructionIndex::Inner(i) => self
                .inner_instructions
                .get_mut(i)
                .unwrap()
                .values_read_mut(),
            InstructionIndex::Terminator => self.terminator.as_mut().unwrap().values_read_mut(),
        }
    }

    pub fn replace_values_written(&mut self, index: InstructionIndex, old: ValueId, new: ValueId) {
        println!("replacing");
        match index {
            InstructionIndex::Phi(i) => self
                .phi_instructions
                .get_mut(i)
                .unwrap()
                .replace_values_written(old, new),
            InstructionIndex::Inner(i) => self
                .inner_instructions
                .get_mut(i)
                .unwrap()
                .replace_values_written(old, new),
            InstructionIndex::Terminator => self
                .terminator
                .as_mut()
                .unwrap()
                .replace_values_written(old, new),
        }
    }

    pub fn replace_values_read(&mut self, index: InstructionIndex, old: ValueId, new: ValueId) {
        match index {
            InstructionIndex::Phi(i) => self
                .phi_instructions
                .get_mut(i)
                .unwrap()
                .replace_values_read(old, new),
            InstructionIndex::Inner(i) => self
                .inner_instructions
                .get_mut(i)
                .unwrap()
                .replace_values_read(old, new),
            InstructionIndex::Terminator => self
                .terminator
                .as_mut()
                .unwrap()
                .replace_values_read(old, new),
        }
    }

    pub fn replace_values(&mut self, index: InstructionIndex, old: ValueId, new: ValueId) {
        match index {
            InstructionIndex::Phi(i) => self
                .phi_instructions
                .get_mut(i)
                .unwrap()
                .replace_values(old, new),
            InstructionIndex::Inner(i) => self
                .inner_instructions
                .get_mut(i)
                .unwrap()
                .replace_values(old, new),
            InstructionIndex::Terminator => {
                self.terminator.as_mut().unwrap().replace_values(old, new)
            }
        }
    }*/

    /*pub(crate) fn value_info(&self, index: InstructionIndex) -> Option<&dyn ValueInfo> {
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
    }*/
}
