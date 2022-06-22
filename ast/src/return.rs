use std::fmt;

#[derive(Debug, Clone, Default)]
pub struct Return {}

impl Return {
    pub fn new() -> Self {
        Self {}
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return")
    }
}
