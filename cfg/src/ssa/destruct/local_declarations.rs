use std::process;

use array_tool::vec::Intersect;
use ast::{LocalRw, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use itertools::{Either, Itertools};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

use crate::{block::BasicBlock, function::Function};

fn push_declaration(block: &mut BasicBlock, local: ast::RcLocal) {
    let mut read_stat_index = None;
    for (index, statement) in block.ast.iter_mut().enumerate() {
        if statement.values_read().contains(&&local) {
            if let Some(index) = read_stat_index {
                let mut assignment =
                    ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
                assignment.prefix = true;
                block.ast.insert(index, assignment.into());
                return;
            }
            read_stat_index = Some(index);
        } else if statement.values_written().contains(&&local) {
            if let Some(assign) = statement.as_assign_mut() {
                assign.prefix = true;
            }
            return;
        }
    }

    if let Some(index) = read_stat_index {
        let mut inline = true;
        let mut local_usages = 0usize;
        for read in block.ast[index].values_read() {
            if read == &local {
                local_usages += 1;
                if local_usages > 1 {
                    inline = false;
                    break;
                }
            }
        }

        if inline {
            if let Some(terminator) = block.terminator() {
                'o: for edge in terminator.edges() {
                    for (_, arg) in &edge.arguments {
                        if arg == &local {
                            local_usages += 1;
                            if local_usages > 1 {
                                inline = false;
                                break 'o;
                            }
                        }
                    }
                }
            }

            if inline {
                // we can replace usage of this local with `nil` :)
                let res = block.ast[index].post_traverse_values(&mut |v| {
                    if let Either::Right(rvalue) = v {
                        if let ast::RValue::Local(rvalue_local) = rvalue && *rvalue_local == local {
                            *rvalue = ast::RValue::Literal(ast::Literal::Nil);
                            return Some(())
                        }
                    }
                    None
                }).is_some();
                assert!(res);
                return;
            }
        }
    }

    let mut declaration = ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
    declaration.prefix = true;
    if block.ast.last().unwrap().as_if().is_some() {
        let index = block.ast.len() - 1;
        block.ast.insert(index, declaration.into());
    } else {
        block.ast.push(declaration.into());
    };
}

fn has_mult_usages_of_local(block: &BasicBlock, local: &ast::RcLocal) -> bool {
    let mut local_usages = 0usize;
    for stat in &block.ast.0 {
        for written in stat.values_written() {
            if written == local {
                local_usages += 1;
                if local_usages > 1 {
                    return true;
                }
            }
        }
        for read in stat.values_read() {
            if read == local {
                local_usages += 1;
                if local_usages > 1 {
                    return true;
                }
            }
        }
    }
    if let Some(terminator) = block.terminator() {
        for edge in terminator.edges() {
            for (_, arg) in &edge.arguments {
                if arg == local {
                    local_usages += 1;
                    if local_usages > 1 {
                        return true;
                    }
                }
            }
        }
    }
    local_usages > 1
}

pub(crate) fn declare_locals(
    function: &mut Function,
    upvalues_in: &IndexSet<ast::RcLocal>,
    local_nodes: FxHashMap<ast::RcLocal, FxHashSet<NodeIndex>>,
    dominators: &Dominators<NodeIndex>,
) {
    for (local, reference_nodes) in local_nodes {
        // TODO: construct IndexSet from parameters?
        if reference_nodes.is_empty()
            || upvalues_in.contains(&local)
            || function.parameters.contains(&local)
        {
            continue;
        }
        if reference_nodes.len() == 1 {
            let reference_node = reference_nodes.into_iter().next().unwrap();
            let block = function.block_mut(reference_node).unwrap();

            push_declaration(block, local);
        } else {
            let mut node_dominators = reference_nodes
                .into_iter()
                .map(|n| dominators.dominators(n).unwrap().collect_vec());
            let mut common_dominators = node_dominators.next().unwrap();
            for node_dominators in node_dominators {
                common_dominators = common_dominators.intersect(node_dominators);
            }
            let common_dominator = common_dominators[0];
            push_declaration(function.block_mut(common_dominator).unwrap(), local);
        }
    }
}
