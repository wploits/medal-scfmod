use crate::{LocalRw, RcLocal, SideEffects, Traverse};

use super::{Block, RValue};

use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: Box<RValue>,
    pub then_block: Option<Block>,
    pub else_block: Option<Block>,
}

impl If {
    pub fn new(condition: RValue, then_block: Option<Block>, else_block: Option<Block>) -> Self {
        Self {
            condition: Box::new(condition),
            then_block,
            else_block,
        }
    }
}

impl Traverse for If {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.condition]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.condition]
    }
}

impl SideEffects for If {
    fn has_side_effects(&self) -> bool {
        true
    }
}

impl LocalRw for If {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.condition.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.condition.values_read_mut()
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} then", self.condition,)?;
        if let Some(then_block) = &self.then_block {
            write!(
                f,
                "\n\t{}",
                then_block
                    .0
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
                    .0
                    .iter()
                    .map(|n| n.to_string().replace('\n', "\n\t"))
                    .join("\n\t")
            )?;
        }
        write!(f, "\nend")
    }
}
