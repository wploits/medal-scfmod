use super::{Block, RValue};

use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub condition: Box<RValue<'a>>,
    pub then_block: Option<Block<'a>>,
    pub else_block: Option<Block<'a>>,
}

impl<'a> If<'a> {
    pub fn new(
        condition: RValue<'a>,
        then_block: Option<Block<'a>>,
        else_block: Option<Block<'a>>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            then_block,
            else_block,
        }
    }
}

impl<'a> fmt::Display for If<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "if {} then\n\t{}",
            self.condition,
            self.then_block
                .iter()
                .map(|n| n.to_string().replace('\n', "\n\t"))
                .join("\n\t")
        )?;
        if let Some(else_block) = &self.else_block {
            writeln!(
                f,
                "else\n\t{}",
                else_block
                    .iter()
                    .map(|n| n.to_string().replace('\n', "\n\t"))
                    .join("\n\t")
            )?;
        }
        write!(f, "end")
    }
}
