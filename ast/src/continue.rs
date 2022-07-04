use std::fmt;

use crate::LocalRw;

#[derive(Debug, Clone)]
pub struct Continue {}

impl LocalRw<'_> for Continue {}

impl fmt::Display for Continue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue")
    }
}
