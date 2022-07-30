use std::fmt;

use crate::{LocalRw, has_side_effects};

#[derive(Debug, Clone)]
pub struct Continue {}

has_side_effects!(Continue);

impl LocalRw for Continue {}

impl fmt::Display for Continue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue")
    }
}
