use derive_more::{Deref, DerefMut, Display, From};
use enum_dispatch::enum_dispatch;
use std::{borrow::Cow, fmt, rc::Rc};

#[derive(Debug, From, Clone, PartialEq)]
pub struct Local<'a>(pub Cow<'a, str>);

impl<'a> Local<'a> {
    pub fn new(name: Cow<'a, str>) -> Self {
        Self(name)
    }
}

impl fmt::Display for Local<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Display, Deref, DerefMut, PartialEq)]
pub struct RcLocal<'a>(Rc<Local<'a>>);

impl<'a> RcLocal<'a> {
    pub fn new(rc: Rc<Local<'a>>) -> Self {
        Self(rc)
    }
}

impl<'a> LocalRw<'a> for RcLocal<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        vec![self]
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        vec![self]
    }
}

#[enum_dispatch]
pub trait LocalRw<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        Vec::new()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        Vec::new()
    }

    fn values_written(&self) -> Vec<&RcLocal<'a>> {
        Vec::new()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        Vec::new()
    }

    fn values(&self) -> Vec<&RcLocal<'a>> {
        let mut res = self.values_read();
        res.extend(self.values_written());
        res
    }

    fn replace_values_read(&mut self, old: &RcLocal<'a>, new: &RcLocal<'a>) {
        for value in self.values_read_mut() {
            if Rc::ptr_eq(value, old) {
                *value = new.clone();
            }
        }
    }

    fn replace_values_written(&mut self, old: &RcLocal<'a>, new: &RcLocal<'a>) {
        for value in self.values_written_mut() {
            if Rc::ptr_eq(value, old) {
                *value = new.clone();
            }
        }
    }

    fn replace_values(&mut self, old: &RcLocal<'a>, new: &RcLocal<'a>) {
        self.replace_values_read(old, new);
        self.replace_values_written(old, new);
    }
}
