use std::fmt;

use crate::LocalRw;

#[derive(Debug, Clone)]
pub struct Break {}

impl LocalRw<'_> for Break {}

impl fmt::Display for Break {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break")
    }
}
