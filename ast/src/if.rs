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
        write!(f, "if {} then", self.condition,)?;
        if let Some(then_block) = &self.then_block {
            write!(
                f,
                "\n\t{}",
                then_block
                    .iter()
                    .map(|n| n.to_string().replace('\n', "\n\t"))
                    .join("\n\t")
            )?;
        }
        if let Some(else_block) = &self.else_block {
            write!(
                f,
                "\nelse\n\t{}",
                else_block
                    .iter()
                    .map(|n| n.to_string().replace('\n', "\n\t"))
                    .join("\n\t")
            )?;
        }
        write!(f, "\nend")
    }
}
