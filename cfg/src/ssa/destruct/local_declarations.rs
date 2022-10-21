use array_tool::vec::Intersect;
use ast::{LocalRw, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use itertools::{Either, Itertools};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

use crate::function::Function;

fn push_declaration(function: &mut Function, node: NodeIndex, local: ast::RcLocal) {
    let block = function.block_mut(node).unwrap();
    let mut read_stat_index = None;
    for (index, statement) in block.iter_mut().enumerate() {
        if statement.values_read().contains(&&local) {
            if read_stat_index.is_some() {
                break;
            }
            read_stat_index = Some(index);
        } else if statement.values_written().contains(&&local) {
            if read_stat_index.is_some() {
                break;
            }
            // TODO: multiple, assigns = sad, lol
            if let Some(assign) = statement.as_assign_mut() && assign.left.len() == 1 {
                assign.prefix = true;
                return;
            }

            read_stat_index = Some(index);
        }
    }

    if let Some(index) = read_stat_index {
        let mut assignment = ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
        assignment.prefix = true;
        block.insert(index, assignment.into());
        return;
    }

    let mut declaration = ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
    declaration.prefix = true;
    // TODO: what about num/generic for loop next?
    if block.last().unwrap().as_if().is_some() {
        let index = block.len() - 1;
        block.insert(index, declaration.into());
    } else {
        block.push(declaration.into());
    };
}

pub(crate) fn declare_locals(
    function: &mut Function,
    upvalues_in: &FxHashSet<ast::RcLocal>,
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
            push_declaration(function, reference_node, local);
        } else {
            let mut node_dominators = reference_nodes
                .into_iter()
                .map(|n| dominators.dominators(n).unwrap().collect_vec());
            let mut common_dominators = node_dominators.next().unwrap();
            for node_dominators in node_dominators {
                common_dominators = common_dominators.intersect(node_dominators);
            }
            let common_dominator = common_dominators[0];
            push_declaration(function, common_dominator, local);
        }
    }
}
