use crate::{SideEffects, Traverse};
use derive_more::{Deref, DerefMut, Display, From};
use enum_dispatch::enum_dispatch;
use std::{fmt, rc::Rc};

#[derive(Debug, From, Clone, PartialEq, Eq, Hash)]
pub struct Local(pub String);

impl Local {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Display, Deref, DerefMut, PartialEq, Eq, Hash)]
pub struct RcLocal(Rc<Local>);

impl SideEffects for RcLocal {}

impl Traverse for RcLocal {}

impl RcLocal {
    pub fn new(rc: Rc<Local>) -> Self {
        Self(rc)
    }
}

impl LocalRw for RcLocal {
    fn values_read(&self) -> Vec<&RcLocal> {
        vec![self]
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        vec![self]
    }
}

#[enum_dispatch]
pub trait LocalRw {
    fn values_read(&self) -> Vec<&RcLocal> {
        Vec::new()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        Vec::new()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        Vec::new()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        Vec::new()
    }

    fn values(&self) -> Vec<&RcLocal> {
        let mut res = self.values_read();
        res.extend(self.values_written());
        res
    }

    fn replace_values_read(&mut self, old: &RcLocal, new: &RcLocal) {
        for value in self.values_read_mut() {
            if Rc::ptr_eq(value, old) {
                *value = new.clone();
            }
        }
    }

    fn replace_values_written(&mut self, old: &RcLocal, new: &RcLocal) {
        for value in self.values_written_mut() {
            if Rc::ptr_eq(value, old) {
                *value = new.clone();
            }
        }
    }

    fn replace_values(&mut self, old: &RcLocal, new: &RcLocal) {
        self.replace_values_read(old, new);
        self.replace_values_written(old, new);
    }
}
