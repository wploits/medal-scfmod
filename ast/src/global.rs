use derive_more::From;
use std::fmt;

use crate::{LocalRw, SideEffects, Traverse};

#[derive(Debug, From, PartialEq, Eq, Clone)]
pub struct Global(pub String);

impl Global {
    pub fn new(name: String) -> Self {
        Self(name)
    }
}

impl LocalRw for Global {}

impl SideEffects for Global {
    fn has_side_effects(&self) -> bool {
        true
    }
}

impl Traverse for Global {}

impl<'a> From<&'a str> for Global {
    fn from(name: &'a str) -> Self {
        Self::new(name.into())
    }
}

impl fmt::Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
