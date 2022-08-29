use crate::{LValue, RValue};
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
pub trait Traverse {
    fn lvalues_mut(&mut self) -> Vec<&mut LValue> {
        Vec::new()
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        Vec::new()
    }

    fn rvalues(&self) -> Vec<&RValue> {
        Vec::new()
    }

    fn traverse_lvalues(
        &mut self,
        lvalue_callback: &impl Fn(&mut LValue),
        rvalue_callback: &impl Fn(&mut RValue),
    ) {
        self.rvalues_mut().into_iter().for_each(rvalue_callback);
        self.lvalues_mut().into_iter().for_each(lvalue_callback);
        self.lvalues_mut().into_iter().for_each(|lvalue| {
            lvalue.traverse_lvalues(lvalue_callback, rvalue_callback);
        });
    }

    fn traverse_rvalues<F>(&mut self, callback: &mut F)
    where
        F: FnMut(&mut RValue),
    {
        for rvalue in self.rvalues_mut() {
            callback(rvalue);
        }
        for rvalue in self.rvalues_mut() {
            rvalue.traverse_rvalues(callback);
        }
    }

    fn post_traverse_rvalues<F>(&mut self, callback: &mut F)
    where
        F: FnMut(&mut RValue),
    {
        for rvalue in self.rvalues_mut() {
            rvalue.traverse_rvalues(callback);
        }
        for rvalue in self.rvalues_mut() {
            callback(rvalue);
        }
    }
}
