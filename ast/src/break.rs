use std::fmt;

use crate::{LocalRw, has_side_effects};

#[derive(Debug, Clone)]
pub struct Break {}

has_side_effects!(Break);

impl LocalRw for Break {}

impl fmt::Display for Break {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break")
    }
}
