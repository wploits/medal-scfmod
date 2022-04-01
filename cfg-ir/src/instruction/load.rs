use std::fmt;

use super::super::constant::Constant;
use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct LoadConstant {
    pub dest: ValueId,
    pub constant: Constant,
}

impl ValueInfo for LoadConstant {
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([self.dest])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.dest])
    }
}

impl fmt::Display for LoadConstant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {}", self.dest, self.constant)
    }
}

#[derive(Debug, Clone)]
pub struct LoadGlobal {
    pub dest: ValueId,
    pub name: String,
}

impl ValueInfo for LoadGlobal {
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        vec![self.dest].into_boxed_slice()
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        vec![&mut self.dest].into_boxed_slice()
    }
}

impl fmt::Display for LoadGlobal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- _G[\"{}\"]", self.dest, self.name)
    }
}

#[derive(Debug, Clone)]
pub struct LoadIndex {
    pub dest: ValueId,
    pub object: ValueId,
    pub key: ValueId,
}

impl ValueInfo for LoadIndex {
    fn values_read(&self) -> Box<[ValueId]> {
        vec![self.object, self.key].into_boxed_slice()
    }

    fn values_written(&self) -> Box<[ValueId]> {
        vec![self.dest].into_boxed_slice()
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        vec![&mut self.object, &mut self.key].into_boxed_slice()
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        vec![&mut self.dest].into_boxed_slice()
    }
}

impl fmt::Display for LoadIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {}[{}]", self.dest, self.object, self.key)
    }
}

#[derive(Debug, Clone)]
pub struct LoadTable {
    pub dest: ValueId,
}

impl ValueInfo for LoadTable {
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        vec![self.dest].into_boxed_slice()
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        vec![&mut self.dest].into_boxed_slice()
    }
}

impl fmt::Display for LoadTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {{}}", self.dest)
    }
}
