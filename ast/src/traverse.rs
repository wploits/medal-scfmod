use crate::{LValue, RValue};
use enum_dispatch::enum_dispatch;
use itertools::Either;

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

    fn post_traverse_rvalues<F, R>(&mut self, callback: &mut F) -> Option<R>
    where
        F: FnMut(&mut RValue) -> Option<R>,
    {
        for rvalue in self.rvalues_mut() {
            if let Some(res) = rvalue.post_traverse_rvalues(callback) {
                return Some(res);
            }
        }
        for rvalue in self.rvalues_mut() {
            if let Some(res) = callback(rvalue) {
                return Some(res);
            }
        }

        None
    }

    fn post_traverse_values<F, R>(&mut self, callback: &mut F) -> Option<R>
    where
        F: FnMut(Either<&mut LValue, &mut RValue>) -> Option<R>,
    {
        for lvalue in self.lvalues_mut() {
            if let Some(res) = lvalue.post_traverse_values(callback) {
                return Some(res);
            }
        }
        for rvalue in self.rvalues_mut() {
            if let Some(res) = rvalue.post_traverse_values(callback) {
                return Some(res);
            }
        }

        for lvalue in self.lvalues_mut() {
            if let Some(res) = callback(Either::Left(lvalue)) {
                return Some(res);
            }
        }
        for rvalue in self.rvalues_mut() {
            if let Some(res) = callback(Either::Right(rvalue)) {
                return Some(res);
            }
        }

        None
    }
}
