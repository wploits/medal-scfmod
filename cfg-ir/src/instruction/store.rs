use std::borrow::Cow;
use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct StoreGlobal<'cfg> {
    pub value: ValueId,
    pub name: Cow<'cfg, str>,
}

impl ValueInfo for StoreGlobal<'_> {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.value]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.value]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl fmt::Display for StoreGlobal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "_G[\"{}\"] <- {}", self.name, self.value)
    }
}

#[derive(Debug, Clone)]
pub struct StoreIndex {
    pub value: ValueId,
    pub object: ValueId,
    pub key: ValueId,
}

impl ValueInfo for StoreIndex {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.value, self.object, self.key]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.value, &mut self.object, &mut self.key]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl fmt::Display for StoreIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}] <- {}", self.object, self.key, self.value)
    }
}
