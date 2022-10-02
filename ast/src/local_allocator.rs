use std::rc::Rc;

use crate::{Local, RcLocal};
#[derive(Debug, Default, Clone)]
pub struct LocalAllocator {
    #[cfg(not(feature = "no_local_debug"))]
    next: usize,
}

impl LocalAllocator {
    #[cfg(not(feature = "no_local_debug"))]
    pub fn allocate(&mut self) -> RcLocal {
        self.next += 1;
        RcLocal::new(Local::new(Some(format!("l_{}", self.next))))
    }

    #[cfg(feature = "no_local_debug")]
    pub fn allocate(&mut self) -> RcLocal {
        RcLocal::new(Local::new(None))
    }
}
