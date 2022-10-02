use std::rc::Rc;

use crate::{Block, RValue, Statement, Traverse};

struct Namer {
    rename: bool,
    counter: usize,
}

impl Namer {
    fn name_locals(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            // TODO: traverse_values
            statement.post_traverse_values(&mut |value| -> Option<()> {
                if let itertools::Either::Right(RValue::Closure(closure)) = value {
                    self.name_locals(&mut closure.body);
                };
                None
            });
            match statement {
                Statement::Assign(assign) if assign.prefix => {
                    for lvalue in &assign.left {
                        let local = lvalue.as_local().unwrap();
                        if self.rename || local.0 .0.borrow().0.is_none() {
                            // TODO: hacky and doesnt work for parameters
                            if Rc::strong_count(local) == 1 {
                                local.0 .0.borrow_mut().0 = Some("_".to_string());
                            } else {
                                local.0 .0.borrow_mut().0 = Some(format!("l_{}", self.counter));
                                self.counter += 1;
                            }
                        }
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
                    self.name_locals(&mut numeric_for.block);
                }
                Statement::GenericFor(generic_for) => {
                    self.name_locals(&mut generic_for.block);
                }
                _ => {}
            }
        }
    }
}

pub fn name_locals(block: &mut Block, rename: bool) {
    Namer { rename, counter: 1 }.name_locals(block);
}
