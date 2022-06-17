use std::borrow::Cow;
use std::fmt;

use super::super::constant::Constant;
use super::super::value::ValueId;
use super::value_info::ValueInfo;

#[derive(Debug, Clone)]
pub struct LoadConstant<'cfg> {
    pub dest: ValueId,
    pub constant: Constant<'cfg>,
}

impl ValueInfo for LoadConstant<'_> {
    fn values_read(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for LoadConstant<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {}", self.dest, self.constant)
    }
}

#[derive(Debug, Clone)]
pub struct LoadGlobal<'cfg> {
    pub dest: ValueId,
    pub name: Cow<'cfg, str>,
}

impl ValueInfo for LoadGlobal<'_> {
    fn values_read(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for LoadGlobal<'_> {
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
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.object, self.key]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.object, &mut self.key]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
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
    fn values_read(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.dest]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.dest]
    }
}

impl fmt::Display for LoadTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {{}}", self.dest)
    }
}
