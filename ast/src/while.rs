use crate::{Block, LocalRw, RValue, RcLocal, has_side_effects, Traverse};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, Clone)]
pub struct While {
    pub condition: RValue,
    pub block: Block,
}

has_side_effects!(While);

impl While {
    pub fn new(condition: RValue, block: Block) -> Self {
        Self { condition, block }
    }
}

impl Traverse for While {
    fn rvalues<'a>(&'a mut self) -> Vec<&'a mut RValue> {
        vec![&mut self.condition]
    }
}

impl LocalRw for While {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.condition.values_read()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.condition.values_read_mut()
    }
}

impl fmt::Display for While {
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
