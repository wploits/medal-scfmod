use std::collections::{HashMap, HashSet};

use crate::{
    instruction::{location::InstructionLocation, value_info::ValueInfo},
    value::ValueId, function::Function,
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
    pub(crate) fn new(function: &Function) -> Self {
        let mut values = HashMap::new();

        for (&node, block) in function.blocks() {
            for &index in block.indices().iter() {
                let value_info = block.value_info(index).unwrap();
                for value_read in value_info.values_read() {
                    values.entry(value_read).or_insert_with(InstructionDefUse::new).reads.insert(
                        InstructionLocation(node, index),
                    );
                }
                for value_written in value_info.values_written() {
                    values.entry(value_written).or_insert_with(InstructionDefUse::new).writes.insert(
                        InstructionLocation(node, index),
                    );
                }
            }
        }

        Self {
            values
        }
    }

    pub fn get(&self, value: ValueId) -> Option<&InstructionDefUse> {
        self.values.get(&value)
    }
}
