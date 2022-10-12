use petgraph::{algo::toposort, stable_graph::NodeIndex, visit::EdgeRef, Direction};

use crate::{function::Function, ssa::param_dependency_graph::ParamDependencyGraph};

// dewolf: Improving Decompilation by leveraging User Surveys (https://arxiv.org/pdf/2205.06719.pdf), page 6 & 7
// https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/phi_dependency_resolver.py
pub fn resolve(function: &mut Function) {
    for node in function.graph().node_indices().collect::<Vec<_>>() {
        let mut dependency_graph = ParamDependencyGraph::new(function, node);
        let directed_fvs = dependency_graph.compute_directed_fvs();
        for dependency_graph_node in directed_fvs {
            remove_dependency(function, node, &mut dependency_graph, dependency_graph_node);
        }

        let topological_order = toposort(&dependency_graph.graph, None)
            .unwrap()
            .into_iter()
            .map(|n| dependency_graph.graph.node_weight(n).unwrap().clone())
            .collect::<Vec<_>>();

        for edge in function
            .graph()
            .edges_directed(node, Direction::Incoming)
            .map(|e| e.id())
            .collect::<Vec<_>>()
        {
            let edge = function.graph_mut().edge_weight_mut(edge).unwrap();
            edge.arguments = topological_order
                .iter()
                .cloned()
                .map(|l| {
                    (
                        l.clone(),
                        edge.arguments
                            .iter()
                            .find(|(k, _)| k == &l)
                            .unwrap()
                            .1
                            .clone(),
                    )
                })
                .collect();
        }
    }
}

fn remove_dependency(
    function: &mut Function,
    node: NodeIndex,
    dependency_graph: &mut ParamDependencyGraph,
    dependency_graph_node: NodeIndex,
) {
    // I'm unsure if this function will be called, so there's a println here :)
    println!("WE ARE REMOVING A DEPENDENCY!!");

    // TODO: do we want to call them locals or variables?? consistent plz
    let successors = dependency_graph
        .graph
        .neighbors(dependency_graph_node)
        .collect::<Vec<_>>();
    let variable = dependency_graph.remove_node(dependency_graph_node).unwrap();
    let name = variable
        .0
         .0
        .borrow()
        .0
        .as_ref()
        .map(|n| format!("copy_{}", n));
    let copy_variable = ast::RcLocal::new(ast::Local::new(name));
    let new_dependency_graph_node = dependency_graph.add_node(copy_variable.clone());
    for successor in successors {
        dependency_graph
            .graph
            .add_edge(new_dependency_graph_node, successor, ());
    }

    for edge in function
        .graph()
        .edges_directed(node, Direction::Incoming)
        .map(|e| e.id())
        .collect::<Vec<_>>()
    {
        if let Some(arg_pair) = function
            .graph_mut()
            .edge_weight_mut(edge)
            .unwrap()
            .arguments
            .iter_mut()
            .find(|(k, _)| k == &variable)
        {
            arg_pair.0 = copy_variable.clone();
        }
    }

    function.block_mut(node).unwrap().insert(
        0,
        ast::Assign::new(vec![variable.into()], vec![copy_variable.into()]).into(),
    )
}

// TODO: fix
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::block::{BasicBlockEdge, Terminator};

//     #[test]
//     fn test_resolve_dependencies() {
//         let mut function = Function::default();

//         let local_y1 = ast::RcLocal::new(ast::Local::new("y1".to_string().into()));
//         let local_y2 = ast::RcLocal::new(ast::Local::new("y2".to_string().into()));
//         let local_z1 = ast::RcLocal::new(ast::Local::new("z1".to_string().into()));
//         let local_z2 = ast::RcLocal::new(ast::Local::new("z2".to_string().into()));

//         let entry_node = function.new_block();
//         let block1_node = function.new_block();
//         let block2_node = function.new_block();
//         function.set_entry(entry_node);

//         let arguments = vec![(local_y2.clone(), local_y1), (local_z2.clone(), local_z1)];
//         function.set_block_terminator(
//             entry_node,
//             Some(Terminator::Jump(BasicBlockEdge {
//                 node: block1_node,
//                 arguments,
//             })),
//         );

//         function.set_block_terminator(
//             block1_node,
//             Some(Terminator::Jump(BasicBlockEdge {
//                 node: block2_node,
//                 arguments: Vec::new(),
//             })),
//         );

//         let arguments = vec![
//             (local_y2.clone(), local_z2.clone()),
//             (local_z2.clone(), local_y2.clone()),
//         ];
//         function.set_block_terminator(
//             block2_node,
//             Some(Terminator::Jump(BasicBlockEdge {
//                 node: block1_node,
//                 arguments,
//             })),
//         );

//         crate::dot::render_to(&function, &mut std::io::stdout()).unwrap();
//         resolve(&mut function);
//         crate::dot::render_to(&function, &mut std::io::stdout()).unwrap();

//         todo!();
//     }
// }
