use rustc_hash::{FxHashMap, FxHashSet};
use triomphe::Arc;
use itertools::Itertools;

use crate::{Block, RValue, RcLocal, Statement, Traverse, Upvalue, Call, MethodCall, Literal, Index, Assign};

struct Namer {
    rename: bool,
    counter: usize,
    upvalues: FxHashSet<RcLocal>,
    name_hints: FxHashMap<RcLocal, String>,
}

impl Namer {
    fn name_local(&mut self, prefix: &str, local: &RcLocal) {
        let mut lock = local.0 .0.lock();
        if self.rename || lock.0.is_none() {
            if let Some(hint) = self.name_hints.get(local) {
                let name_prefix = if self.upvalues.contains(local) { "_u_" } else { "" };
                lock.0 = Some(format!("{}{}", name_prefix, hint));
            }
            else if Arc::count(&local.0 .0) == 1 {
                lock.0 = Some("_".to_string());
            } else {
                let name_prefix = if self.upvalues.contains(local) { "_u_" } else { "" };
                lock.0 = Some(format!("{}{}{}", prefix, name_prefix, self.counter));
                self.counter += 1;
            }
        }
    }

    fn collect_name_hints(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            if let Statement::Assign(assign) = statement {
                if assign.left.len() == 1 && assign.right.len() == 1 {
                    if let Some(lvalue_local) = assign.left[0].as_local() {
                        let rvalue = &assign.right[0];
                        if let Some(hint) = self.get_hint_from_rvalue(rvalue) {
                            self.name_hints.insert(lvalue_local.clone(), hint);
                        }
                    }
                }
            }
            
            // Add hint for table containing RemoteEvent/RemoteFunction
            if let Statement::GenericFor(generic_for) = statement {
                if generic_for.res_locals.len() == 2 {
                    if let Some(local_v2) = generic_for.right.get(0).and_then(|r| r.as_local()) {
                        if self.name_hints.get(local_v2) == Some(&"remotes".to_string()) {
                            if let Some(local_v4) = generic_for.res_locals.get(1) {
                                self.name_hints.insert(local_v4.clone(), "remote".to_string());
                            }
                        }
                    }
                }
            }

            match statement {
                Statement::If(r#if) => {
                    self.collect_name_hints(&mut r#if.then_block.lock());
                    self.collect_name_hints(&mut r#if.else_block.lock());
                },
                Statement::While(r#while) => self.collect_name_hints(&mut r#while.block.lock()),
                Statement::Repeat(repeat) => self.collect_name_hints(&mut repeat.block.lock()),
                Statement::NumericFor(numeric_for) => self.collect_name_hints(&mut numeric_for.block.lock()),
                Statement::GenericFor(generic_for) => self.collect_name_hints(&mut generic_for.block.lock()),
                _ => {},
            }
        }
    }

    fn get_hint_from_rvalue(&self, rvalue: &RValue) -> Option<String> {
        match rvalue {
            RValue::Index(index) => match &*index.right {
                RValue::Literal(Literal::String(string)) => {
                    if let Ok(s) = std::str::from_utf8(string) {
                        Some(s.to_string())
                    } else {
                        None
                    }
                }
                _ => None,
            },
            RValue::Global(global) => {
                if let Ok(s) = std::str::from_utf8(&global.0) {
                    Some(s.to_string())
                } else {
                    None
                }
            }
            RValue::Call(Call { value, arguments: _ }) => {
                if let Some(MethodCall { value: _, method, arguments: args }) = value.as_method_call() {
                    if method == "GetService" && args.len() == 1 {
                        if let Some(RValue::Literal(Literal::String(arg_str))) = args.get(0) {
                            if let Ok(s) = std::str::from_utf8(arg_str) {
                                return Some(s.to_string());
                            }
                        }
                    }
                }
                None
            }
            RValue::MethodCall(MethodCall { value: _, method, arguments }) => {
                if method == "GetService" && arguments.len() == 1 {
                    if let Some(RValue::Literal(Literal::String(arg_str))) = arguments.get(0) {
                        if let Ok(s) = std::str::from_utf8(arg_str) {
                            return Some(s.to_string());
                        }
                    }
                }
                None
            }
            RValue::Table(_) => Some("remotes".to_string()),
            _ => None,
        }
    }

    fn name_locals(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            statement.post_traverse_values(&mut |value| -> Option<()> {
                if let itertools::Either::Right(RValue::Closure(closure)) = value {
                    let mut function = closure.function.lock();
                    for param in &function.parameters {
                        self.name_local("p", param);
                    }
                    self.name_locals(&mut function.body);
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
                    self.name_locals(&mut r#if.then_block.lock());
                    self.name_locals(&mut r#if.else_block.lock());
                }
                Statement::While(r#while) => {
                    self.name_locals(&mut r#while.block.lock());
                }
                Statement::Repeat(repeat) => {
                    self.name_locals(&mut repeat.block.lock());
                }
                Statement::NumericFor(numeric_for) => {
                    self.name_local("v", &numeric_for.counter);
                    self.name_locals(&mut numeric_for.block.lock());
                }
                Statement::GenericFor(generic_for) => {
                    for res_local in &generic_for.res_locals {
                        self.name_local("v", res_local);
                    }
                    self.name_locals(&mut generic_for.block.lock());
                }
                _ => {}
            }
        }
    }

    fn find_upvalues(&mut self, block: &mut Block) {
        for statement in &mut block.0 {
            statement.post_traverse_values(&mut |value| -> Option<()> {
                if let itertools::Either::Right(RValue::Closure(closure)) = value {
                    self.upvalues.extend(
                        closure
                            .upvalues
                            .iter()
                            .map(|u| match u {
                                Upvalue::Copy(l) | Upvalue::Ref(l) => l,
                            })
                            .cloned(),
                    );
                    self.find_upvalues(&mut closure.function.lock().body);
                };
                None
            });
            match statement {
                Statement::If(r#if) => {
                    self.find_upvalues(&mut r#if.then_block.lock());
                    self.find_upvalues(&mut r#if.else_block.lock());
                }
                Statement::While(r#while) => {
                    self.find_upvalues(&mut r#while.block.lock());
                }
                Statement::Repeat(repeat) => {
                    self.find_upvalues(&mut repeat.block.lock());
                }
                Statement::NumericFor(numeric_for) => {
                    self.find_upvalues(&mut numeric_for.block.lock());
                }
                Statement::GenericFor(generic_for) => {
                    self.find_upvalues(&mut generic_for.block.lock());
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
        name_hints: FxHashMap::default(),
    };
    namer.find_upvalues(block);
    namer.collect_name_hints(block);
    namer.name_locals(block);
}
