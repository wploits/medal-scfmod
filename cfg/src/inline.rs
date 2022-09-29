use crate::function::Function;
use ast::{LocalRw, SideEffects, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexMap;
use itertools::Either;

fn assigns<'a>(
    block: &'a ast::Block,
    locals_out: &FxHashSet<&ast::RcLocal>,
    check_statement: usize,
    ref_local: &ast::RcLocal,
) -> Option<&'a ast::RValue> {
    if let ast::Statement::Assign(assign) = &block[check_statement] {
        if assign.left.len() == 1 && assign.right.len() == 1 {
            if let ast::LValue::Local(local) = &assign.left[0] {
                if local == ref_local && !locals_out.contains(local) {
                    return Some(&assign.right[0]);
                }
            }
        }
    }
    None
}

fn inline_expression(
    statement: &mut ast::Statement,
    read: &ast::RcLocal,
    new_expression: ast::RValue,
) {
    let mut new_expression = Some(new_expression);
    statement.post_traverse_values(&mut |v| {
        if let Either::Right(rvalue) = v {
            if let ast::RValue::Local(rvalue_local) = rvalue && *rvalue_local == *read {
                *rvalue = new_expression.take().unwrap();
                return Some(())
            }
        }
        None
    });
}

// TODO: dont clone expressions
// TODO: move to ssa module?
fn inline_expressions(function: &mut Function, upvalue_to_group: &IndexMap<ast::RcLocal, usize>) {
    let node_indices = function.graph().node_indices().collect::<Vec<_>>();
    let mut local_usages = FxHashMap::default();
    for &node in &node_indices {
        let block = function.block(node).unwrap();
        for stat in &block.ast.0 {
            for read in stat.values_read() {
                *local_usages.entry(read.clone()).or_insert(0usize) += 1;
            }
        }
        if let Some(terminator) = block.terminator() {
            for edge in terminator.edges() {
                for (_, arg) in &edge.arguments {
                    *local_usages.entry(arg.clone()).or_insert(0usize) += 1;
                }
            }
        }
    }
    for node in node_indices {
        let block = function.block_mut(node).unwrap();
        let mut locals_out = FxHashSet::default();
        if let Some(terminator) = &block.terminator {
            locals_out.extend(
                terminator
                    .edges()
                    .into_iter()
                    .flat_map(|e| e.arguments.iter().map(|(_, a)| a)),
            );
        }

        // TODO: rename values_read to locals_read
        let mut stat_to_values_read = Vec::new();
        for stat in &block.ast.0 {
            stat_to_values_read.push(
                stat.values_read()
                    .into_iter()
                    .filter(|&l| local_usages[l] == 1 && !upvalue_to_group.contains_key(l))
                    .cloned()
                    .collect::<Vec<_>>(),
            );
        }

        let mut index = 0;
        'w: while index < block.ast.len() {
            for stat_index in (0..index).rev() {
                for read in &stat_to_values_read[index] {
                    if let ast::Statement::Assign(assign) = &block.ast[stat_index]
                        && assign.left.len() == 1
                        && assign.right.len() == 1
                        && let ast::LValue::Local(local) = &assign.left[0]
                        && local == read
                        && !locals_out.contains(local)
                    {
                        let new_expression = &assign.right[0];
                        if !new_expression.has_side_effects() || block.ast[index].rvalues().into_iter().find_map(|rvalue| {
                            if rvalue.values_read().contains(&read) {
                                Some(true)
                            } else if rvalue.has_side_effects() {
                                Some(false)
                            } else {
                                None
                            }
                        }).unwrap_or(false) {
                            let new_expression = std::mem::replace(
                                &mut block.ast[stat_index],
                                ast::Empty {}.into())
                                .into_assign().unwrap().right.into_iter().next().unwrap();
                            inline_expression(&mut block.ast[index], read, new_expression);
                            block.ast[stat_index] = ast::Empty {}.into();
                            continue 'w;
                        }
                    }
                }
            }
            index += 1;
        }
    }
}

pub fn inline(function: &mut Function, upvalue_to_group: &IndexMap<ast::RcLocal, usize>) {
    let mut changed = true;
    while changed {
        changed = false;
        inline_expressions(function, upvalue_to_group);
        // we check block.ast.len() elsewhere and do `i - ` here and elsewhere so we need to get rid of empty statements
        // TODO: fix ^
        for (_, block) in function.blocks_mut() {
            block.ast.retain(|s| s.as_empty().is_none());
        }
        for (_, block) in function.blocks_mut() {
            // if the first statement is a setlist, we cant inline it anyway
            for i in 1..block.ast.len() {
                if let ast::Statement::SetList(setlist) = &block.ast[i] {
                    let table_local = setlist.table.clone();
                    if let Some(assign)= block.ast.get_mut(i - 1).unwrap().as_assign_mut() && assign.left == [table_local.into()]
                    {
                        let setlist = std::mem::replace(block.ast.get_mut(i).unwrap(), ast::Empty {}.into()).into_set_list().unwrap();
                        let assign = block.ast.get_mut(i - 1).unwrap().as_assign_mut().unwrap();
                        let table = assign.right[0].as_table_mut().unwrap();
                        assert!(table.0.len() == setlist.index - 1);
                        for value in setlist.values {
                            table.0.push(value);
                        }
                        if let Some(tail) = setlist.tail {
                            table.0.push(tail);
                        }
                        changed = true;
                    }
                    // todo: only inline in changed blocks
                    //cfg::dot::render_to(function, &mut std::io::stdout());
                    //break 'outer;
                }
            }
        }
    }
    // we check block.ast.len() elsewhere and do `i - ` here and elsewhere so we need to get rid of empty statements
    // TODO: fix ^
    for (_, block) in function.blocks_mut() {
        block.ast.retain(|s| s.as_empty().is_none());
    }
}
