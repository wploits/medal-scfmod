use crate::function::Function;
use ast::{LocalRw, SideEffects, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexMap;
use itertools::Either;

fn inline_expression(
    statement: &mut ast::Statement,
    read: &ast::RcLocal,
    new_expression: ast::RValue,
    new_expr_has_side_effects: bool,
) -> bool {
    let mut new_expression = Some(new_expression);
    statement
        .post_traverse_values(&mut |v| {
            if let Either::Right(rvalue) = v {
                if let ast::RValue::Local(rvalue_local) = rvalue && *rvalue_local == *read {
                    *rvalue = new_expression.take().unwrap();
                    // success!
                    return Some(true)
                }
                if new_expr_has_side_effects && rvalue.has_side_effects() {
                    // failure :(
                    return Some(false);
                }
            }

            // keep searching
            None
        })
        .unwrap_or(false)
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
                    .filter(|&l| {
                        local_usages[l] == 1
                            && !upvalue_to_group.contains_key(l)
                            && !locals_out.contains(l)
                    })
                    .cloned()
                    .map(Some)
                    .collect::<Vec<_>>(),
            );
        }

        let mut index = 0;
        'w: while index < block.ast.len() {
            let mut allow_side_effects = true;
            for stat_index in (0..index).rev() {
                let mut values_read = stat_to_values_read[index]
                    .iter_mut()
                    .filter(|l| l.is_some())
                    .peekable();
                if values_read.peek().is_none() {
                    index += 1;
                    continue 'w;
                }
                if let ast::Statement::Assign(assign) = &block.ast[stat_index]
                    && assign.left.len() == 1
                    && assign.right.len() == 1
                    && let ast::LValue::Local(local) = &assign.left[0]
                {
                    let new_expression = &assign.right[0];
                    let new_expr_has_side_effects = new_expression.has_side_effects();
                    let local = local.clone();
                    for read_opt in values_read {
                        let read = read_opt.as_ref().unwrap();
                        if read != &local {
                            continue;
                        };
                        if !new_expr_has_side_effects || allow_side_effects {
                            let new_expression = block.ast[stat_index].as_assign().unwrap().right[0].clone();
                            if inline_expression(&mut block.ast[index], read, new_expression, new_expr_has_side_effects) {
                                block.ast[stat_index] = ast::Empty {}.into();
                                *read_opt = None;
                                continue 'w;
                            }
                        }
                    }
                }
                allow_side_effects &= !block.ast[stat_index].has_side_effects();
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
        for (_, block) in function.blocks_mut() {
            // we check block.ast.len() elsewhere and do `i - ` here and elsewhere so we need to get rid of empty statements
            // TODO: fix ^
            block.ast.retain(|s| s.as_empty().is_none());

            let mut i = 0;
            while i < block.ast.len() {
                if let ast::Statement::Assign(assign) = &block.ast[i]
                    && assign.left.len() == 1
                    && assign.right.len() == 1
                    && assign.right[0].as_table().is_some()
                    && let ast::LValue::Local(table_local) = &assign.left[0]
                {
                    let table_index = i;
                    let table_local = table_local.clone();
                    i += 1;
                    while i < block.ast.len()
                        && let ast::Statement::Assign(field_assign) = &block.ast[i]
                        && field_assign.left.len() == 1
                        && field_assign.right.len() == 1
                        && let ast::LValue::Index(ast::Index {
                            left: box ast::RValue::Local(local),
                            ..
                        }) = &field_assign.left[0]
                        && local == &table_local
                    {
                        let field_assign = std::mem::replace(block.ast.get_mut(i).unwrap(), ast::Empty {}.into()).into_assign().unwrap();
                        block.ast[table_index].as_assign_mut().unwrap().right.get_mut(0).unwrap().as_table_mut().unwrap().0.push((Some(Box::into_inner(field_assign.left.into_iter().next().unwrap().into_index().unwrap().right)), field_assign.right.into_iter().next().unwrap()));
                        changed = true;
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }

            // if the first statement is a setlist, we cant inline it anyway
            for i in 1..block.ast.len() {
                if let ast::Statement::SetList(setlist) = &block.ast[i] {
                    let table_local = setlist.table.clone();
                    if let Some(assign)= block.ast.get_mut(i - 1).unwrap().as_assign_mut() && assign.left == [table_local.into()]
                    {
                        let setlist = std::mem::replace(block.ast.get_mut(i).unwrap(), ast::Empty {}.into()).into_set_list().unwrap();
                        let assign = block.ast.get_mut(i - 1).unwrap().as_assign_mut().unwrap();
                        let table = assign.right[0].as_table_mut().unwrap();
                        assert!(table.0.iter().filter(|(k, _)| k.is_none()).count() == setlist.index - 1);
                        for value in setlist.values {
                            table.0.push((None, value));
                        }
                        // table already has tail?
                        assert!(!table.0.last().map_or(false, |(k, v)| k.is_none() && matches!(v, ast::RValue::VarArg(_) | ast::RValue::Call(_) | ast::RValue::MethodCall(_))));
                        if let Some(tail) = setlist.tail {
                            table.0.push((None, tail));
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
