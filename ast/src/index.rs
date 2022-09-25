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
    fn values_read(&self) -> Vec<&RcLocal> {
        self.left
            .values_read()
            .into_iter()
            .chain(self.right.values_read().into_iter())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.left
            .values_read_mut()
            .into_iter()
            .chain(self.right.values_read_mut().into_iter())
            .collect()
    }
}

impl Traverse for Index {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.left, &mut self.right]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.left, &self.right]
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            // TODO: this is repeated in call
            match self.left.as_ref() {
                RValue::Local(_) | RValue::Global(_) | RValue::Index(_) => self.left.to_string(),
                _ => "(".to_string() + &self.left.to_string() + ")",
            },
            match self.right.as_ref() {
                RValue::Literal(super::Literal::String(field))
                    if field.is_ascii()
                        && field
                            .chars()
                            .enumerate()
                            .all(|(i, c)| (i != 0 && c.is_ascii_digit())
                                || c.is_ascii_alphabetic()
                                || c == '_') =>
                    format!(".{}", field),
                _ => format!("[{}]", self.right),
            }
        )
    }
}
