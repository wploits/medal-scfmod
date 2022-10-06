use crate::{formatter::Formatter, LocalRw, RcLocal, SideEffects, Traverse};

use super::{Block, RValue};

use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: RValue,
    pub then_block: Option<Block>,
    pub else_block: Option<Block>,
}

impl If {
    pub fn new(condition: RValue, then_block: Option<Block>, else_block: Option<Block>) -> Self {
        Self {
            condition,
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
        Formatter {
            indentation_level: 0,
            indentation_mode: Default::default(),
            output: f,
        }
        .format_if(self)
    }
}
