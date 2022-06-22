use crate::{Block, RValue};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone)]
pub struct While<'a> {
    condition: RValue<'a>,
    block: Block<'a>,
}

impl<'a> While<'a> {
    pub fn new(condition: RValue<'a>, block: Block<'a>) -> Self {
        Self { condition, block }
    }
}

impl fmt::Display for While<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "while {} do\n\t{}\nend",
            self.condition,
            self.block
                .iter()
                .map(|n| n.to_string().replace('\n', "\n\t"))
                .join("\n\t")
        )
    }
}
