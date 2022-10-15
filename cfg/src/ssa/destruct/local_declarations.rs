use array_tool::vec::Intersect;
use ast::{LocalRw, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use itertools::{Either, Itertools};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

use crate::function::Function;

fn push_declaration(
    function: &mut Function,
    node: NodeIndex,
    local: ast::RcLocal,
    mut inline: bool,
) {
    let block = function.block_mut(node).unwrap();
    let mut read_stat_index = None;
    for (index, statement) in block.iter_mut().enumerate() {
        if statement.values_read().contains(&&local) {
            if read_stat_index.is_some() {
                inline = false;
                break;
            }
            read_stat_index = Some(index);
        } else if statement.values_written().contains(&&local) {
            if read_stat_index.is_some() {
                inline = false;
                break;
            }
            if let Some(assign) = statement.as_assign_mut() {
                assign.prefix = true;
            }
            return;
        }
    }

    if let Some(index) = read_stat_index {
        let mut local_usages = 0usize;
        if inline {
            for read in block[index].values_read() {
                if read == &local {
                    local_usages += 1;
                    if local_usages > 1 {
                        inline = false;
                        break;
                    }
                }
            }
        }

        if inline {
            'o: for edge in function.edges(node) {
                for (_, arg) in &edge.weight().arguments {
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

        let block = function.block_mut(node).unwrap();
        if inline {
            // we can replace usage of this local with `nil` :)
            let res = block[index]
                .post_traverse_values(&mut |v| {
                    if let Either::Right(rvalue) = v {
                        if let ast::RValue::Local(rvalue_local) = rvalue && *rvalue_local == local {
                            *rvalue = ast::RValue::Literal(ast::Literal::Nil);
                            return Some(())
                        }
                    }
                    None
                })
                .is_some();
            // res can be false in cases where the local is read, but there is no RValue local
            /*
            local a
            local function b()
                return a
            end
            */
            if res {
                return;
            }
        }

        let mut assignment = ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
        assignment.prefix = true;
        block.insert(index, assignment.into());
        return;
    }

    let mut declaration = ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
    declaration.prefix = true;
    if block.last().unwrap().as_if().is_some() {
        let index = block.len() - 1;
        block.insert(index, declaration.into());
    } else {
        block.push(declaration.into());
    };
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
            push_declaration(function, reference_node, local, true);
        } else {
            let mut node_dominators = reference_nodes
                .into_iter()
                .map(|n| dominators.dominators(n).unwrap().collect_vec());
            let mut common_dominators = node_dominators.next().unwrap();
            for node_dominators in node_dominators {
                common_dominators = common_dominators.intersect(node_dominators);
            }
            let common_dominator = common_dominators[0];
            push_declaration(function, common_dominator, local, false);
        }
    }
}
