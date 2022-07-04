use crate::{Block, LocalRw, RValue, RcLocal};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone)]
pub struct While<'a> {
    pub condition: RValue<'a>,
    pub block: Block<'a>,
}

impl<'a> While<'a> {
    pub fn new(condition: RValue<'a>, block: Block<'a>) -> Self {
        Self { condition, block }
    }
}

impl<'a> LocalRw<'a> for While<'a> {
    fn values_read(&self) -> Vec<&RcLocal<'a>> {
        self.condition.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal<'a>> {
        self.condition.values_read_mut()
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
