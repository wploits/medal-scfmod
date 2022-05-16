use std::collections::{HashMap, HashSet};

use crate::{
    instruction::{location::InstructionLocation, value_info::ValueInfo},
    value::ValueId,
};

#[derive(Debug, Clone)]
pub struct InstructionDefUse {
    pub reads: HashSet<InstructionLocation>,
    pub writes: HashSet<InstructionLocation>,
}

impl InstructionDefUse {
    fn new() -> Self {
        Self {
            reads: HashSet::new(),
            writes: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefUse {
    values: HashMap<ValueId, InstructionDefUse>,
}

impl DefUse {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn xref(&self, value: ValueId) -> Option<&InstructionDefUse> {
        self.values.get(&value)
    }

    pub(crate) fn register<T: ValueInfo>(&mut self, location: &InstructionLocation, info: &T) {
        for &value_read in info.values_read().iter() {
            self.values
                .entry(value_read)
                .or_insert_with(InstructionDefUse::new)
                .reads
                .insert(*location);
        }
        for &value_written in info.values_written().iter() {
            self.values
                .entry(value_written)
                .or_insert_with(InstructionDefUse::new)
                .writes
                .insert(*location);
        }
    }

    pub(crate) fn unregister(&mut self, location: &InstructionLocation) {
        for value in self.values.values_mut() {
            value.reads.remove(location);
            value.writes.remove(location);
        }
    }
}
