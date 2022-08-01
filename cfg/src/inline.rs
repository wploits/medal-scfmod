use crate::function::Function;
use ast::{LocalRw, SideEffects, Traverse};
use graph::NodeId;

fn assigns(
    block: &ast::Block,
    check_statement: usize,
    ref_local: &ast::RcLocal,
) -> Option<ast::RValue> {
    if let ast::Statement::Assign(assign) = &block[check_statement] {
        if assign.left.len() == 1 && assign.right.len() == 1 {
            if let ast::LValue::Local(local) = &assign.left[0] {
                if local == ref_local {
                    return Some(assign.right[0].clone());
                }
            }
        }
    }
    None
}

pub fn inline_expressions(function: &mut Function, node: NodeId) {
    let block = function.block_mut(node).unwrap();
    let mut index = 0;
    while index < block.len() {
        for read in block[index]
            .values_read()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>()
        {
            for stat_index in (0..index).rev() {
                if let Some(new_expression) = assigns(block, stat_index, &read) {
                    if block[stat_index + 1..index]
                        .iter()
                        .any(|statement| statement.has_side_effects())
                    {
                        break;
                    }
                    block.get_mut(index).unwrap().traverse_rvalues(&|rvalue| {
                        if let ast::RValue::Local(rvalue_local) = rvalue {
                            if *rvalue_local == read {
                                *rvalue = new_expression.clone();
                            }
                        }
                    });
                    block.remove(stat_index);
                    index -= 1;
                    continue;
                }
            }
        }
        index += 1;
    }
}
