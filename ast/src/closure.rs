use std::fmt;

use itertools::Itertools;

use crate::{
    formatter::Formatter,
    type_system::{Infer, TypeSystem},
    Block, LocalRw, RcLocal, SideEffects, Traverse, Type,
};

#[derive(Debug, PartialEq, Clone)]
pub enum Upvalue {
    Copy(RcLocal),
    Ref(RcLocal),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub parameters: Vec<RcLocal>,
    pub upvalues: Vec<Upvalue>,
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
        Formatter {
            indentation_level: 0,
            indentation_mode: Default::default(),
            output: f,
        }
        .format_closure(self)
    }
}

impl LocalRw for Closure {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.upvalues.iter().map(|u| match u {
            Upvalue::Copy(l)
            | Upvalue::Ref(l) => l,
        }).collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.upvalues.iter_mut().map(|u| match u {
            Upvalue::Copy(l)
            | Upvalue::Ref(l) => l,
        }).collect()
    }
}

impl SideEffects for Closure {}

impl Traverse for Closure {}
