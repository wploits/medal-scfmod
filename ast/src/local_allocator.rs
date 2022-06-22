use std::rc::Rc;

use crate::Local;

#[derive(Debug, Default, Clone)]
pub struct LocalAllocator {
    next: usize,
}

impl LocalAllocator {
    pub fn allocate<'a>(&mut self) -> Rc<Local<'a>> {
        self.next += 1;
        Rc::new(Local::new(format!("l_{}", self.next).into()))
    }
}
