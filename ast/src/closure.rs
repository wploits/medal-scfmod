use std::{collections::HashMap, fmt};

use itertools::Itertools;

use crate::{
    type_system::{Infer, TypeSystem},
    Block, LocalRw, RcLocal, SideEffects, Statement, Traverse, Type,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub parameters: Vec<RcLocal>,
    pub upvalues: Vec<RcLocal>,
    pub body: Block,
}

impl Infer for Closure {
    fn infer<'a: 'b, 'b>(&'a mut self, system: &mut TypeSystem<'b>) -> Type {
        let return_values = system.analyze_block(&mut self.body);
        let parameters = self
            .parameters
            .iter_mut()
            .map(|l| l.infer(system))
            .collect_vec();

        Type::Function(parameters, return_values)
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "function({})\n{}\nend",
            self.parameters.iter().join(", "),
            self.body.0.iter().join("\n"),
        )
    }
}

impl LocalRw for Closure {}

impl SideEffects for Closure {}

impl Traverse for Closure {}
