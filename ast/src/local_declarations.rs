use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Sub,
};

use indexmap::IndexSet;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{Assign, Block, Literal, LocalRw, NumericFor, RcLocal, Statement};

fn collect_block_locals<'a>(block: &'a Block, locals: &mut IndexSet<&'a RcLocal>) {
    for stat in &block.0 {
        collect_stat_locals(stat, locals);
    }
}

fn collect_stat_locals<'a>(stat: &'a Statement, locals: &mut IndexSet<&'a RcLocal>) {
    locals.extend(stat.values().into_iter());
    // TODO: traverse_values
    match stat {
        Statement::If(r#if) => {
            collect_block_locals(&r#if.then_block, locals);
            collect_block_locals(&r#if.else_block, locals);
        }
        Statement::While(r#while) => {
            collect_block_locals(&r#while.block, locals);
        }
        Statement::Repeat(repeat) => {
            collect_block_locals(&repeat.block, locals);
        }
        Statement::NumericFor(numeric_for) => {
            collect_block_locals(&numeric_for.block, locals);
        }
        Statement::GenericFor(generic_for) => {
            collect_block_locals(&generic_for.block, locals);
        }
        _ => {}
    }
}

fn block_has_local(block: &Block, local: &RcLocal) -> bool {
    for stat in &block.0 {
        if stat_has_local(stat, local) {
            return true;
        }
    }
    false
}

fn stat_has_local(stat: &Statement, local: &RcLocal) -> bool {
    if stat.values().contains(&local) {
        return true;
    }
    // TODO: traverse_values
    match stat {
        Statement::If(r#if) => {
            if block_has_local(&r#if.then_block, local) {
                return true;
            }
            if block_has_local(&r#if.else_block, local) {
                return true;
            }
        }
        Statement::While(r#while) => {
            if block_has_local(&r#while.block, local) {
                return true;
            }
        }
        Statement::Repeat(repeat) => {
            if block_has_local(&repeat.block, local) {
                return true;
            }
        }
        Statement::NumericFor(numeric_for) => {
            if block_has_local(&numeric_for.block, local) {
                return true;
            }
        }
        Statement::GenericFor(generic_for) => {
            if block_has_local(&generic_for.block, local) {
                return true;
            }
        }
        _ => {}
    }
    false
}

pub fn declare_local(block: &mut Block, local: &RcLocal) {
    let mut usages = Vec::new();
    for (stat_index, stat) in block.iter().enumerate() {
        if stat_has_local(stat, local) {
            usages.push(stat_index);
            match usages.len() {
                1 if !matches!(
                    stat,
                    Statement::If(_)
                        | Statement::While(_)
                        | Statement::Repeat(_)
                        | Statement::NumericFor(_)
                        | Statement::GenericFor(_)
                ) =>
                {
                    break
                }
                1 => continue,
                2 => break,
                _ => unreachable!(),
            }
        }
    }

    let mut usages = usages.into_iter();
    let first_stat_index = usages.next().unwrap();
    let declared = if usages.next().is_none() {
        // single usage in this block, declare the local inside the statement
        // if possible
        match &mut block[first_stat_index] {
            Statement::If(r#if) if !r#if.values().into_iter().contains(local) => {
                let then_contains_local = block_has_local(&r#if.then_block, local);
                let else_contains_local = block_has_local(&r#if.else_block, local);
                if then_contains_local && !else_contains_local {
                    declare_local(&mut r#if.then_block, local);
                    true
                } else if else_contains_local && !then_contains_local {
                    declare_local(&mut r#if.else_block, local);
                    true
                } else {
                    false
                }
            }
            Statement::While(r#while) if !r#while.values().into_iter().contains(local) => {
                declare_local(&mut r#while.block, local);
                true
            }
            Statement::Repeat(repeat) => {
                declare_local(&mut repeat.block, local);
                true
            }
            Statement::NumericFor(numeric_for)
                if numeric_for.values_written().into_iter().contains(local) =>
            {
                true
            }
            Statement::GenericFor(generic_for)
                if generic_for.values_written().into_iter().contains(local) =>
            {
                true
            }
            Statement::NumericFor(numeric_for)
                if !numeric_for.values().into_iter().contains(local) =>
            {
                declare_local(&mut numeric_for.block, local);
                true
            }
            Statement::GenericFor(generic_for)
                if !generic_for.values().into_iter().contains(local) =>
            {
                declare_local(&mut generic_for.block, local);
                true
            }
            _ => false,
        }
    } else {
        false
    };

    if !declared {
        // we still need to declare the local
        match &mut block[first_stat_index] {
            stat @ Statement::NumericFor(_) | stat @ Statement::GenericFor(_)
                if stat.values_written().into_iter().contains(local) =>
            {
                unreachable!()
            }
            Statement::Assign(assign)
                if assign
                    .left
                    .iter()
                    .exactly_one()
                    .ok()
                    .and_then(|l| l.as_local())
                    == Some(local) =>
            {
                assign.prefix = true;
            }
            _ => {
                if first_stat_index > 0 && let Statement::Assign(assign) = &mut block[first_stat_index - 1] && assign.prefix && assign.right.is_empty() {
                    assign.left.push(local.clone().into());
                    // TODO: unnecessary clone, use iter_mut or smthn to take mut ref to two
                    let declared = assign.left.iter().map(|l| l.as_local().unwrap()).cloned().collect::<FxHashSet<_>>();

                    if let Statement::Assign(assign) = &mut block[first_stat_index] {
                        if assign.left.iter().all(|l| l.as_local().is_some_and(|l| declared.contains(l))) {
                            assign.prefix = true;
                            block.remove(first_stat_index - 1);
                        }
                    }
                } else {
                    let mut declaration = Assign::new(vec![local.clone().into()], vec![]);
                    declaration.prefix = true;
                    block.insert(first_stat_index, declaration.into());
                };
            }
        }
    }
}

pub fn declare_locals(block: &mut Block, locals_to_ignore: &FxHashSet<RcLocal>) {
    let mut locals = IndexSet::new();
    collect_block_locals(block, &mut locals);
    for local in locals
        .into_iter()
        .filter(|l| !locals_to_ignore.contains(l))
        .cloned()
        .collect_vec()
    {
        declare_local(block, &local);
    }
}
