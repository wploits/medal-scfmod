use std::fmt;

#[derive(Debug, Clone)]
pub struct Continue {}

impl fmt::Display for Continue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "continue")
    }
}
