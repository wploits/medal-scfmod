use std::fmt;

use itertools::Itertools;

use crate::{
    type_system::{Infer, TypeSystem},
    Block, LocalRw, RcLocal, SideEffects, Traverse, Type,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub parameters: Vec<RcLocal>,
    // TODO: bad way of keeping track
    pub id: usize,
    pub upvalues: Vec<RcLocal>,
    pub body: Block,
    pub is_variadic: bool,
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
            "function({})\n-- id: {}\n-- upvalues: {}\n{}\nend",
            self.parameters.iter().join(", "),
            self.id,
            self.upvalues.iter().join(", "),
            self.body.0.iter().join("\n"),
        )
    }
}

impl LocalRw for Closure {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.upvalues.iter().collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.upvalues.iter_mut().collect()
    }
}

impl SideEffects for Closure {}

impl Traverse for Closure {}
