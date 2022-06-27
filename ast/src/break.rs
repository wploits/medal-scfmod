use std::fmt;

#[derive(Debug, Clone)]
pub struct Break {}

impl fmt::Display for Break {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "break")
    }
}
