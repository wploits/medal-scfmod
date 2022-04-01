use graph::NodeId;
use std::collections::HashMap;
use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

// A pseudo instruction sometimes used when blocks merge in SSA form
#[derive(Debug, Clone)]
pub struct Phi {
    pub dest: ValueId,
    // { block: value }
    pub incoming_values: HashMap<NodeId, ValueId>,
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
            "{} <- Φ({})",
            self.dest,
            self.incoming_values
                .iter()
                .map(|(_, v)| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
