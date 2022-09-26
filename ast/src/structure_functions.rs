use std::collections::HashMap;

use crate::{Block, RValue, RcLocal, Statement, Traverse, LValue};

struct Structurer {
    function_count: usize,
    function_stack: Vec<(Block, Vec<RcLocal>, Vec<RcLocal>)>,
    // TODO: fxhashmap
    structured: HashMap<usize, (Block, Vec<RcLocal>, Vec<RcLocal>)>,
}

pub fn replace_locals(block: &mut Block, map: &HashMap<RcLocal, RcLocal>) {
    for statement in &mut block.0 {
        // TODO: traverse_values
        statement.post_traverse_values(&mut |value| -> Option<()> {
            match value {
                itertools::Either::Left(LValue::Local(local)) 
                | itertools::Either::Right(RValue::Local(local)) => {
                    if let Some(new_local) = map.get(local) {
                        *local = new_local.clone();
                    }
                },
                itertools::Either::Right(RValue::Closure(closure)) => replace_locals(&mut closure.body, map),
                _ => {}
            };
            None
        });
        match statement {
            Statement::If(r#if) => {
                if let Some(b) = &mut r#if.then_block {
                    replace_locals(b, map);
                }

                if let Some(b) = &mut r#if.else_block {
                    replace_locals(b, map);
                }
            }
            Statement::While(r#while) => {
                replace_locals(&mut r#while.block, map);
            }
            Statement::NumericFor(numeric_for) => {
                replace_locals(&mut numeric_for.block, map);
            }
            Statement::GenericFor(generic_for) => {
                replace_locals(&mut generic_for.block, map);
            }
            _ => {}
        }
    }
}

impl Structurer {
    pub fn structure(mut self) -> Block {
        while let Some((mut block, params, upvalues)) = self.function_stack.pop() {
            self.visit_block(&mut block);
            self.structured.insert(
                self.function_count - self.function_stack.len() - 1,
                (block, params, upvalues),
            );
        }
        self.structured
            .remove(&(self.function_count - 1))
            .unwrap()
            .0
    }

    pub fn visit_block(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            statement.traverse_rvalues(&mut |rvalue| {
                if let Some(closure) = rvalue.as_closure_mut() {
                    let (body, params, upvalues) = &self.structured[&closure.id];
                    let mut body = body.clone();
                    let mut local_map = HashMap::with_capacity(upvalues.len());
                    for (old, new) in upvalues.iter().zip(&closure.upvalues) {
                        println!("{:?} -> {:?}", old, new);
                        local_map.insert(old.clone(), new.clone());
                    }
                    replace_locals(&mut body, &local_map);
                    closure.body = body;
                    closure.parameters = params.clone();
                }
            });
            match statement {
                Statement::If(r#if) => {
                    if let Some(b) = &mut r#if.then_block {
                        self.visit_block(b);
                    }

                    if let Some(b) = &mut r#if.else_block {
                        self.visit_block(b);
                    }
                }
                Statement::While(r#while) => {
                    self.visit_block(&mut r#while.block);
                }
                Statement::NumericFor(numeric_for) => {
                    self.visit_block(&mut numeric_for.block);
                }
                Statement::GenericFor(generic_for) => {
                    self.visit_block(&mut generic_for.block);
                }
                _ => {}
            }
        }
    }
}

pub fn structure_functions(mut functions: Vec<(Block, Vec<RcLocal>, Vec<RcLocal>)>) -> Block {
    functions.reverse();
    Structurer {
        function_count: functions.len(),
        function_stack: functions,
        structured: HashMap::new(),
    }
    .structure()
}
