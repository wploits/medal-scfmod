use std::fmt;

use itertools::Itertools;

use crate::{Block, LocalRw, RcLocal, SideEffects, Statement, Traverse};

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub parameters: Vec<RcLocal>,
    pub upvalues: Vec<RcLocal>,
    pub body: Block,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "function({})\n\t{}\nend",
            self.parameters.iter().join(", "),
            self.body
                .0
                .iter()
                .map(|n| n.to_string().replace('\n', "\n\t"))
                .join("\n\t"),
        )
    }
}

impl LocalRw for Closure {}

impl SideEffects for Closure {}

impl Traverse for Closure {}
