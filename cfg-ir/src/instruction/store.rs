use std::fmt;

use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct StoreGlobal {
    pub value: ValueId,
    pub name: String,
}

impl ValueInfo for StoreGlobal {
    fn values_read(&self) -> Box<[ValueId]> {
        vec![self.value].into_boxed_slice()
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        vec![&mut self.value].into_boxed_slice()
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }
}

impl fmt::Display for StoreGlobal {
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
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([self.value, self.object, self.key])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.value, &mut self.object, &mut self.key])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }
}

impl fmt::Display for StoreIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}] <- {}", self.object, self.key, self.value)
    }
}
