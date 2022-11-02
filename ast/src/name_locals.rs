use std::rc::Rc;

use rustc_hash::FxHashSet;

use crate::{Block, RValue, RcLocal, Statement, Traverse};

struct Namer {
    rename: bool,
    counter: usize,
    upvalues: FxHashSet<RcLocal>,
}

impl Namer {
    fn name_local(&mut self, prefix: &str, local: &RcLocal) {
        if self.rename || local.0 .0.borrow().0.is_none() {
            // TODO: hacky and slow
            if Rc::strong_count(&local.0.0) == 1 {
                local.0 .0.borrow_mut().0 = Some("_".to_string());
            } else {
                let prefix = prefix.to_string()
                    + if self.upvalues.contains(local) {
                        "_u_"
                    } else {
                        ""
                    };
                local.0 .0.borrow_mut().0 = Some(format!("{}{}", prefix, self.counter));
                self.counter += 1;
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
                    self.name_locals(&mut r#if.then_block);
                    self.name_locals(&mut r#if.else_block);
                }
                Statement::While(r#while) => {
                    self.name_locals(&mut r#while.block);
                }
                Statement::Repeat(repeat) => {
                    self.name_locals(&mut repeat.block);
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

    fn find_upvalues(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            // TODO: traverse_values
            statement.post_traverse_values(&mut |value| -> Option<()> {
                if let itertools::Either::Right(RValue::Closure(closure)) = value {
                    self.upvalues.extend(closure.upvalues.iter().cloned());
                    self.find_upvalues(&mut closure.body);
                };
                None
            });
            match statement {
                Statement::If(r#if) => {
                    self.find_upvalues(&mut r#if.then_block);
                    self.find_upvalues(&mut r#if.else_block);
                }
                Statement::While(r#while) => {
                    self.find_upvalues(&mut r#while.block);
                }
                Statement::Repeat(repeat) => {
                    self.find_upvalues(&mut repeat.block);
                }
                Statement::NumericFor(numeric_for) => {
                    self.find_upvalues(&mut numeric_for.block);
                }
                Statement::GenericFor(generic_for) => {
                    self.find_upvalues(&mut generic_for.block);
                }
                _ => {}
            }
        }
    }
}

pub fn name_locals(block: &mut Block, rename: bool) {
    let mut namer = Namer {
        rename,
        counter: 1,
        upvalues: FxHashSet::default(),
    };
    namer.find_upvalues(block);
    namer.name_locals(block);
}
