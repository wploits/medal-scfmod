use std::{collections::HashMap, rc::Rc};

use crate::{Block, RValue, RcLocal, Statement, Traverse};

struct Namer {
    rename: bool,
    counter: HashMap<String, usize>,
}

impl Namer {
    fn name_local(&mut self, prefix: &str, local: &RcLocal) {
        if self.rename || local.0 .0.borrow().0.is_none() {
            // TODO: hacky
            if Rc::strong_count(local) == 1 {
                local.0 .0.borrow_mut().0 = Some("_".to_string());
            } else {
                let counter = self.counter.entry(prefix.to_string()).or_insert(1);
                local.0 .0.borrow_mut().0 = Some(format!("{}{}", prefix, counter));
                *counter += 1;
            }
        }
    }

    fn name_locals(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            // TODO: traverse_values
            statement.post_traverse_values(&mut |value| -> Option<()> {
                if let itertools::Either::Right(RValue::Closure(closure)) = value {
                    for param in &closure.parameters {
                        self.name_local("p", param);
                    }
                    self.name_locals(&mut closure.body);
                };
                None
            });
            match statement {
                Statement::Assign(assign) if assign.prefix => {
                    for lvalue in &assign.left {
                        self.name_local("v", lvalue.as_local().unwrap());
                    }
                }
                Statement::If(r#if) => {
                    if let Some(b) = &mut r#if.then_block {
                        self.name_locals(b);
                    }

                    if let Some(b) = &mut r#if.else_block {
                        self.name_locals(b);
                    }
                }
                Statement::While(r#while) => {
                    self.name_locals(&mut r#while.block);
                }
                Statement::NumericFor(numeric_for) => {
                    self.name_local("v", &numeric_for.counter);
                    self.name_locals(&mut numeric_for.block);
                }
                Statement::GenericFor(generic_for) => {
                    for res_local in &generic_for.res_locals {
                        self.name_local("v", res_local);
                    }
                    self.name_locals(&mut generic_for.block);
                }
                _ => {}
            }
        }
    }
}

pub fn name_locals(block: &mut Block, rename: bool) {
    Namer {
        rename,
        counter: HashMap::new(),
    }
    .name_locals(block);
}
