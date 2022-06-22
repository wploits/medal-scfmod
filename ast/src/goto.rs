use derive_more::From;
use std::{borrow::Cow, fmt};

#[derive(Debug, Clone, From)]
pub struct Label<'a>(pub Cow<'a, str>);

impl<'a> From<&'a str> for Label<'a> {
    fn from(str: &'a str) -> Self {
        Label(str.into())
    }
}

impl<'a> From<String> for Label<'a> {
    fn from(str: String) -> Self {
        Label(str.into())
    }
}

impl fmt::Display for Label<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "__{}:", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Goto<'a>(pub Label<'a>);

impl<'a> Goto<'a> {
    pub fn new(label: Label<'a>) -> Self {
        Self(label)
    }
}

impl fmt::Display for Goto<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "goto {}", self.0)
    }
}
