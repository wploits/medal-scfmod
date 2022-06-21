use derive_more::From;
use std::{borrow::Cow, fmt};

#[derive(Debug, From, Clone)]
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
