use crate::{RValue, LValue};
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait Traverse {    
    fn lvalues<'a>(&'a mut self) -> Vec<&'a mut LValue> {
        Vec::new()
    }

    fn rvalues<'a>(&'a mut self) -> Vec<&'a mut RValue> {
        Vec::new()
    }

    fn traverse_rvalues(&mut self, callback: &impl Fn(&mut RValue)) {
        self.rvalues().into_iter().for_each(callback);
        self.rvalues().into_iter().for_each(|rvalue| {
            rvalue.traverse_rvalues(callback);
        });
    }
}