use crate::function::Function;
use ast::{LocalRw, SideEffects, Traverse};
use fxhash::FxHashSet;
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
pub fn inline_expressions(function: &mut Function, node: NodeIndex) {
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
                .collect::<Vec<_>>()
            {
                let mut use_count = 0;
                block
                    .ast
                    .get_mut(index)
                    .unwrap()
                    .post_traverse_rvalues(&mut |rvalue| {
                        if let ast::RValue::Local(rvalue_local) = rvalue {
                            if *rvalue_local == read {
                                use_count += 1;
                            }
                        }
                    });
                if use_count > 1 {
                    continue;
                }

                if let Some(new_expression) = assigns(&block.ast, &locals_out, stat_index, &read) {
                    let new_has_side_effects = new_expression.has_side_effects();
                    if new_has_side_effects
                        && block.ast[stat_index + 1..index]
                            .iter()
                            .any(|s| s.has_side_effects())
                    {
                        break;
                    }
                    let statement = block.ast.get_mut(index).unwrap();
                    let rvalues = match statement {
                        ast::Statement::Assign(assign) => {
                            assert_eq!(assign.right.len(), 1);
                            assign.right.get_mut(0).unwrap().rvalues_mut()
                        }
                        ast::Statement::If(r#if) => r#if.condition.rvalues_mut(),
                        _ => statement.rvalues_mut(),
                    };
                    for rvalue in rvalues {
                        if new_has_side_effects && rvalue.has_side_effects() {
                            break;
                        }
                        if let ast::RValue::Local(rvalue_local) = rvalue {
                            if *rvalue_local == read {
                                *rvalue = new_expression;
                                block.ast.remove(stat_index);
                                index -= 1;
                                continue 'outer;
                            }
                        }
                    }
                }
            }
        }
        index += 1;
    }
}
