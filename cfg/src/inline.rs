use crate::function::Function;
use ast::{LocalRw, SideEffects, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use itertools::Either;
use petgraph::stable_graph::NodeIndex;

fn assigns(
    block: &ast::Block,
    locals_out: &FxHashSet<&ast::RcLocal>,
    check_statement: usize,
    ref_local: &ast::RcLocal,
) -> Option<ast::RValue> {
    if let ast::Statement::Assign(assign) = &block[check_statement] {
        if assign.left.len() == 1 && assign.right.len() == 1 {
            if let ast::LValue::Local(local) = &assign.left[0].0 {
                if local == ref_local && !locals_out.contains(local) {
                    return Some(assign.right[0].clone());
                }
            }
        }
    }
    None
}

// TODO: move to ssa module?
pub fn inline_expressions(function: &mut Function) {
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
        let mut index = 0;
        while index < block.ast.len() {
            'outer: for stat_index in (0..index).rev() {
                for read in block.ast[index]
                    .values_read()
                    .into_iter()
                    .cloned()
                    .collect::<FxHashSet<_>>()
                {
                    if local_usages[&read] > 1 {
                        continue;
                    }

                    if let Some(new_expression) =
                        assigns(&block.ast, &locals_out, stat_index, &read)
                    {
                        println!("{:?}", new_expression);
                        let new_has_side_effects = new_expression.has_side_effects();
                        if new_has_side_effects
                            && block.ast[stat_index + 1..index]
                                .iter()
                                .any(|s| s.has_side_effects())
                        {
                            break;
                        }
                        let statement = block.ast.get_mut(index).unwrap();
                        if let Some(replaced) = statement.post_traverse_values(&mut |v| {
                            if let Either::Right(rvalue) = v {
                                if new_has_side_effects && rvalue.has_side_effects() {
                                    Some(false)
                                } else if let ast::RValue::Local(rvalue_local) = rvalue
                                    && *rvalue_local == read {
                                        *rvalue = new_expression.clone();
                                        Some(true)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }) {
                            if replaced {
                                block.ast.remove(stat_index);
                                index -= 1;
                                continue 'outer;
                            }
                        }
                    }
                }
            }
            index += 1;
        }
    }
}
