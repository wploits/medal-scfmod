use fxhash::FxHashMap;
use graph::NodeId;
use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

// A pseudo instruction sometimes used when blocks merge in SSA form
#[derive(Debug, Clone)]
pub struct Phi {
    pub dest: ValueId,
    pub incoming_values: FxHashMap<NodeId, ValueId>,
}

impl ValueInfo for Phi {
    fn values_read(&self) -> Box<[ValueId]> {
        self.incoming_values.values().cloned().collect()
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([self.dest])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        self.incoming_values.values_mut().collect()
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.dest])
    }
}

impl fmt::Display for Phi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            // TODO: create a pull request for the dot repo so it doesnt escape unicode characters
            // like our 'Φ' symbol
            "{} <- phi({})",
            self.dest,
            self.incoming_values
                .iter()
                .map(|(_, v)| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
