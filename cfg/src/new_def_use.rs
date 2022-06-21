use fxhash::FxHashMap;
use graph::{algorithms::dfs_tree, NodeId};

use crate::{
    function::Function,
    instruction::{
        location::{InstructionIndex, InstructionLocation},
        value_info::ValueInfo,
    },
    value::ValueId,
};

#[derive(Debug, Default)]
pub struct ValueDefUse {
    pub reads: Vec<InstructionLocation>,
    pub writes: Vec<InstructionLocation>,
}

#[derive(Debug, Default)]
pub struct DefUse {
    values: Vec<ValueId>,
    access_by_index: Vec<ValueDefUse>,
    value_to_index: FxHashMap<ValueId, usize>,
}

impl DefUse {
    /// Returns all the values that were discovered in the function.
    pub fn collected_values(&self) -> &Vec<ValueId> {
        &self.values
    }

    pub fn access_by_node(&self, node: NodeId) -> Vec<(ValueId, &ValueDefUse)> {
        self.access_by_index
            .iter()
            .enumerate()
            .filter(|(_, value_def_use)| {
                value_def_use
                    .reads
                    .iter()
                    .any(|location| location.node == node)
                    || value_def_use
                        .writes
                        .iter()
                        .any(|location| location.node == node)
            })
            .map(|(index, value_def_use)| (self.values[index], value_def_use))
            .collect()
    }

    /// Returns the locations where the given value is accessed.
    pub fn access_by_index(&self, index: usize) -> &ValueDefUse {
        debug_assert!(self.values.len() > index);
        &self.access_by_index[index]
    }

    /// Returns the locations where the given value is accessed.
    pub fn access_by_value(&self, value: &ValueId) -> &ValueDefUse {
        debug_assert!(self.value_to_index.contains_key(value));
        &self.access_by_index[self.value_to_index[value]]
    }

    /// Returns the indices of the values accessed by the instruction.
    pub fn indices_by_location(&self, location: &InstructionLocation) -> Vec<usize> {
        self.access_by_index
            .iter()
            .enumerate()
            .filter(|(_, value)| value.reads.contains(location) || value.writes.contains(location))
            .map(|(index, _)| index)
            .collect()
    }

    /// Creates a new [`DefUse`].
    pub fn capture(function: &Function) -> Self {
        let graph = function.graph();
        let root = function.entry().unwrap();
        let dfs = dfs_tree(graph, root).unwrap();

        let instruction_accesses = dfs
            .pre_order(root)
            .unwrap()
            .iter()
            .flat_map(|&node| {
                let block = function.block(node).unwrap();
                let phi = block
                    .phi_instructions
                    .iter()
                    .enumerate()
                    .map(|(index, phi)| {
                        (
                            InstructionIndex::Phi(index),
                            phi.values_read(),
                            phi.values_written(),
                        )
                    });
                let inner = block
                    .inner_instructions
                    .iter()
                    .enumerate()
                    .map(|(index, inner)| {
                        (
                            InstructionIndex::Inner(index),
                            inner.values_read(),
                            inner.values_written(),
                        )
                    });
                let terminator = block.terminator().iter().map(|terminator| {
                    (
                        InstructionIndex::Terminator,
                        terminator.values_read(),
                        terminator.values_written(),
                    )
                });
                phi.chain(inner)
                    .chain(terminator)
                    .map(move |(index, reads, writes)| {
                        (InstructionLocation { node, index }, reads, writes)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let values = instruction_accesses
            .iter()
            .flat_map(|(_, reads, writes)| reads.iter().chain(writes.iter()).cloned())
            .collect::<Vec<_>>();

        let mut access_by_index = Vec::with_capacity(values.len());
        for _ in 0..values.len() {
            access_by_index.push(ValueDefUse::default())
        }
        let mut value_to_index = FxHashMap::default();
        for (location, reads, writes) in instruction_accesses {
            for read in reads {
                let index = *value_to_index
                    .entry(read)
                    .or_insert_with(|| values.iter().position(|&value| value == read).unwrap());
                access_by_index[index].reads.push(location);
            }
            for write in writes {
                let index = *value_to_index
                    .entry(write)
                    .or_insert_with(|| values.iter().position(|&value| value == write).unwrap());
                access_by_index[index].writes.push(location);
            }
        }

        Self {
            values,
            access_by_index,
            value_to_index,
        }
    }
}
