use super::{Literal, RValue};

pub trait NameGenerator {
    fn generate_name(&self, rvalue: &RValue, identifier: usize) -> Option<String>;
}

pub struct DefaultNameGenerator {}

impl NameGenerator for DefaultNameGenerator {
    fn generate_name(&self, rvalue: &RValue, identifier: usize) -> Option<String> {
        let hint = match rvalue {
            RValue::Global(global) => Some(global.to_string()),
            RValue::Index(index) => match &*index.right {
                RValue::Literal(Literal::String(string)) => Some(format!("{}", string)),
                _ => None,
            },
            _ => None,
        };
        hint.map(|hint| format!("v_{}_{}", identifier, hint))
    }
}
