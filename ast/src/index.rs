use crate::{has_side_effects, LocalRw, RcLocal, Traverse};

use super::RValue;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub left: Box<RValue>,
    pub right: Box<RValue>,
}

has_side_effects!(Index);

impl Index {
    pub fn new(left: RValue, right: RValue) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

impl LocalRw for Index {
    fn values_read<'a>(&'a self) -> Box<dyn Iterator<Item = &'a RcLocal> + 'a> {
        Box::new(self.left.values_read().chain(self.right.values_read()))
    }

    fn values_read_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &'a mut RcLocal> + 'a> {
        Box::new(
            self.left
                .values_read_mut()
                .chain(self.right.values_read_mut()),
        )
    }
}

impl Traverse for Index {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.left, &mut self.right]
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.left, match &self.right {
            box RValue::Literal(super::Literal::String(field)) => format!(".{}", field),
            _ => format!("[{}]", self.right),
        })
    }
}
