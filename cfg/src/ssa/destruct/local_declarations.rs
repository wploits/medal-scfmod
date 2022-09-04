use array_tool::vec::Intersect;
use ast::LocalRw;
use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex};

use crate::function::Function;

fn push_declaration(block: &mut ast::Block, local: ast::RcLocal) {
    for (index, statement) in block.iter_mut().enumerate() {
        if statement.values_read().contains(&&local) {
            let mut assignment =
                ast::Assign::new(vec![local.into()], vec![ast::Literal::Nil.into()]);
            assignment.prefix = true;
            block.insert(index, assignment.into());
            return;
        } else if statement.values_written().contains(&&local) {
            if let Some(assign) = statement.as_assign_mut() {
                assign.prefix = true;
            }
            return;
        }
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
    local_nodes: FxHashMap<ast::RcLocal, FxHashSet<NodeIndex>>,
    dominators: &Dominators<NodeIndex>,
) {
    for (local, reference_nodes) in local_nodes {
        if reference_nodes.is_empty() {
            continue;
        }
        let mut node_dominators = reference_nodes
            .into_iter()
            .map(|n| dominators.dominators(n).unwrap().collect_vec());
        let mut common_dominators = node_dominators.next().unwrap();
        for node_dominators in node_dominators {
            common_dominators = common_dominators.intersect(node_dominators);
        }
        let common_dominator = common_dominators[0];
        push_declaration(
            &mut function.block_mut(common_dominator).unwrap().ast,
            local,
        );
    }
}
