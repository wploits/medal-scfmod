use std::rc::Rc;

use crate::{Local, RcLocal};

#[derive(Debug, Default, Clone)]
pub struct LocalAllocator {
    next: usize,
}

impl LocalAllocator {
    pub fn allocate(&mut self) -> RcLocal<'_> {
        self.next += 1;
        RcLocal::new(Rc::new(Local::new(format!("l_{}", self.next).into())))
    }
}
