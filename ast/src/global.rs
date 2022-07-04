use derive_more::From;
use std::{borrow::Cow, fmt};

use crate::LocalRw;

#[derive(Debug, From, Clone)]
pub struct Global<'a>(pub Cow<'a, str>);

impl<'a> Global<'a> {
    pub fn new(name: Cow<'a, str>) -> Self {
        Self(name)
    }
}

impl LocalRw<'_> for Global<'_> {}

impl<'a> From<&'a str> for Global<'a> {
    fn from(name: &'a str) -> Self {
        Self::new(name.into())
    }
}

impl fmt::Display for Global<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
