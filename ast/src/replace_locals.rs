use std::collections::HashMap;

use itertools::Either;

use crate::{Block, LValue, RValue, RcLocal, Statement, Traverse};

pub fn replace_locals<H: std::hash::BuildHasher>(
    block: &mut Block,
    map: &HashMap<RcLocal, RcLocal, H>,
) {
    for statement in &mut block.0 {
        // TODO: traverse_values
        statement.post_traverse_values(&mut |value| -> Option<()> {
            match value {
                Either::Left(LValue::Local(local))
                | Either::Right(RValue::Local(local)) => {
                    if let Some(new_local) = map.get(local) {
                        *local = new_local.clone();
                    }
                }
                Either::Right(RValue::Closure(closure)) => {
                    replace_locals(&mut closure.body, map)
                }
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
