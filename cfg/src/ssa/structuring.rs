use ast::{LocalRw, Reduce, SideEffects, Traverse, UnaryOperation};
use contracts::requires;
use fxhash::FxHashSet;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use petgraph::{
    algo::dominators::Dominators,
    stable_graph::{EdgeIndex, NodeIndex},
    visit::{DfsPostOrder, EdgeRef},
    Direction,
};
use tuple::Map;

use crate::{
    block::{BlockEdge, BranchType},
    function::Function,
};

#[derive(Debug)]
pub enum PatternOperator {
    And,
    Or,
}

impl From<PatternOperator> for ast::BinaryOperation {
    fn from(val: PatternOperator) -> Self {
        match val {
            PatternOperator::And => ast::BinaryOperation::And,
            PatternOperator::Or => ast::BinaryOperation::Or,
        }
    }
}

#[derive(Debug)]
pub struct ConditionalAssignmentPattern {
    assigner: NodeIndex,
    next: NodeIndex,
    tested_local: ast::RcLocal,
    assigned_local: ast::RcLocal,
    assigned_value: ast::RValue,
    parameter: ast::RcLocal,
    operator: PatternOperator,
}

type ConditionalSequenceConfiguration = (bool, bool);

#[derive(Debug)]
pub struct ConditionalSequencePattern {
    first_node: NodeIndex,
    second_node: NodeIndex,
    short_circuit: NodeIndex,
    assign: bool,
    inverted: bool,
    final_condition: ast::RValue,
}

#[derive(Debug)]
pub struct GenericForNextPattern {
    body_node: NodeIndex,
    res_locals: Vec<ast::RcLocal>,
    // generator is not necessarily a local in Luau
    // it is commonly something like: `generator or __get_builtin("iter")`
    generator: ast::RValue,
    state: ast::RcLocal,
    internal_control: ast::RcLocal,
}

fn simplify_condition(function: &mut Function, node: NodeIndex) -> bool {
    let block = function.block_mut(node).unwrap();
    if let Some(if_stat) = block.last_mut().and_then(|s| s.as_if_mut())
        && let Some(unary) = if_stat.condition.as_unary() {
        if_stat.condition = *unary.value.clone();
        let (then_edge, else_edge) = function.conditional_edges(node).unwrap().map(|e| e.id());
        let (then_edge, else_edge) = function.graph_mut().index_twice_mut(then_edge, else_edge);
        then_edge.branch_type = BranchType::Else;
        else_edge.branch_type = BranchType::Then;
        true
    } else {
        false
    }
}

fn single_assign(block: &ast::Block) -> Option<&ast::Assign> {
    if block.len() == 1 && let Some(assign) = block.last().unwrap().as_assign() {
        Some(assign)
    } else {
        None
    }
}

fn match_conditional_sequence(
    function: &Function,
    node: NodeIndex,
) -> Option<ConditionalSequencePattern> {
    // TODO: check if len() == 1?
    let block = function.block(node).unwrap();
    if let Some(r#if) = block.last().and_then(|s| s.as_if()) {
        let first_condition = r#if.condition.clone();
        let test_pattern = |second_conditional, other| {
            let second_conditional_successors =
                function.successor_blocks(second_conditional).collect_vec();
            let second_block = function.block(second_conditional).unwrap();
            if let Some(second_conditional_if) = second_block.last().and_then(|s| s.as_if()) {
                if second_conditional_successors.len() == 2
                    && second_conditional_successors
                        .iter()
                        .filter(|&s| s == &other)
                        .count()
                        == 1
                {
                    if second_block.len() == 2 {
                        if let ast::Statement::Assign(assign) = &second_block[0] {
                            let values_written = assign.values_written();
                            if values_written.len() == 1
                                && second_conditional_if.condition
                                    == values_written[0].clone().into()
                            {
                                assert!(assign.right.len() == 1);
                                return Some((assign.right[0].clone(), true));
                            }
                        }
                        return None;
                    } else if second_block.len() == 1 {
                        return Some((second_conditional_if.condition.clone(), false));
                    }
                }
            }
            None
        };
        let first_terminator = function.conditional_edges(node).unwrap();
        let (then_edge, else_edge) = first_terminator;
        if function.predecessor_blocks(then_edge.target()).count() == 1
            && let Some((second_condition, assign)) = test_pattern(then_edge.target(), else_edge.target())
        {
            let second_terminator = function.conditional_edges(then_edge.target()).unwrap();
            if second_terminator.0.target() == else_edge.target() {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: then_edge.target(),
                    short_circuit: else_edge.target(),
                    assign,
                    inverted: true,
                    final_condition: ast::Binary::new(ast::Unary::new(first_condition, ast::UnaryOperation::Not).into(), second_condition, ast::BinaryOperation::Or).into()
                })
            } else {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: then_edge.target(),
                    short_circuit: else_edge.target(),
                    assign,
                    inverted: false,
                    final_condition: ast::Binary::new(first_condition, second_condition, ast::BinaryOperation::And).into()
                })
            }
        } else if function.predecessor_blocks(else_edge.target()).count() == 1
            && let Some((second_condition, assign)) = test_pattern(else_edge.target(), then_edge.target())
        {
            let second_terminator = function.conditional_edges(else_edge.target()).unwrap();
            if first_terminator.0.target() == second_terminator.0.target() {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: else_edge.target(),
                    short_circuit: then_edge.target(),
                    assign,
                    inverted: false,
                    final_condition: ast::Binary::new(first_condition, second_condition, ast::BinaryOperation::Or).into()
                })
            } else {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: else_edge.target(),
                    short_circuit: then_edge.target(),
                    assign,
                    inverted: true,
                    final_condition: ast::Binary::new(ast::Unary::new(first_condition, ast::UnaryOperation::Not).into(), second_condition, ast::BinaryOperation::And).into()
                })
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn match_conditional_assignment(
    function: &Function,
    node: NodeIndex,
) -> Option<ConditionalAssignmentPattern> {
    if let Some(r#if) = function.block(node).unwrap().last().and_then(|s| s.as_if()) {
        let edges = function.conditional_edges(node).unwrap();

        if let ast::RValue::Local(condition) = &r#if.condition {
            let test_pattern = |assigner, next| {
                if function.successor_blocks(assigner).collect_vec() == [next]
                    && function.predecessor_blocks(assigner).collect_vec() == [node]
                    && let Some(assign) = single_assign(function.block(assigner).unwrap())
                    && assign.left.len() == 1 && assign.right.len() == 1
                    && let ast::LValue::Local(assigned_local) = &assign.left[0]
                    && let Some(assign_edge) = function.unconditional_edge(assigner)
                    && let other_edge = if edges.0.target() == next { edges.0 } else { edges.1 }
                    && let Some(assign_param) = assign_edge.weight().arguments.iter().filter_map(|(p, a)| Some((p, a.as_local()?))).find_map(|(k, v)| if v == assigned_local { Some(k) } else { None })
                    && let Some(other_param) = other_edge.weight().arguments.iter().filter_map(|(p, a)| Some((p, a.as_local()?))).find_map(|(k, v)| if v == condition { Some(k) } else { None })
                    && assign_param == other_param
                {
                    Some((assigned_local.clone(), assign.right[0].clone(), assign_param.clone()))
                } else {
                    None
                }
            };

            let (a, b) = (edges.0.target(), edges.1.target());
            if let Some((assigned_local, assigned_value, parameter)) = test_pattern(a, b) {
                return Some(ConditionalAssignmentPattern {
                    assigner: a,
                    next: b,
                    tested_local: condition.clone(),
                    assigned_local,
                    assigned_value,
                    parameter,
                    operator: PatternOperator::And,
                });
            } else if let Some((assigned_local, assigned_value, parameter)) = test_pattern(b, a) {
                return Some(ConditionalAssignmentPattern {
                    assigner: b,
                    next: a,
                    tested_local: condition.clone(),
                    assigned_local,
                    assigned_value,
                    parameter,
                    operator: PatternOperator::Or,
                });
            }
        }
    }

    None
}

pub fn structure_compound_conditionals(function: &mut Function) -> bool {
    let mut did_structure = false;
    // TODO: does this need to be in dfs post order?
    let mut dfs = DfsPostOrder::new(function.graph(), function.entry().unwrap());
    while let Some(node) = dfs.next(function.graph()) {
        if simplify_condition(function, node) {
            did_structure = true;
        }
        if let Some(pattern) = match_conditional_assignment(function, node)
            // TODO: can we continue?
            && &Some(pattern.assigner) != function.entry()
        {
            let block = function.block_mut(node).unwrap();
            block.pop();
            block.push(
                ast::Assign::new(
                    vec![pattern.assigned_local.clone().into()],
                    vec![ast::Binary::new(
                        pattern.tested_local.into(),
                        pattern.assigned_value,
                        pattern.operator.into(),
                    )
                    .into()],
                )
                .into(),
            );

            // TODO: remove_conditional_edges
            let edges = function.conditional_edges(node).unwrap();
            // TODO: name?
            let the_edge = if edges.0.target() == pattern.next { edges.0 } else { edges.1 }.id();
            let mut arguments = function.graph_mut().edge_weight_mut(the_edge).unwrap().arguments.clone();
            for (parameter, argument) in arguments.iter_mut() {
                if *parameter == pattern.parameter {
                    *argument = pattern.assigned_local.into();
                    break;
                }
            }
            function.remove_block(pattern.assigner);
            function.set_edges(node, vec![(pattern.next, BlockEdge { branch_type: BranchType::Unconditional, arguments })]);

            did_structure = true;
        }

        if let Some(pattern) = match_conditional_sequence(function, node)
            // TODO: can we continue?
            && &Some(pattern.second_node) != function.entry()
        {
            let second_to_sc_edges = function.edges(pattern.second_node).filter(|e| e.target() == pattern.short_circuit).collect::<Vec<_>>();
            assert!(second_to_sc_edges.len() == 1);
            let second_to_sc_args = second_to_sc_edges[0].weight().arguments.clone();
            let first_to_sc_edges = function.edges(pattern.first_node).filter(|e| e.target() == pattern.short_circuit).collect::<Vec<_>>();
            assert!(first_to_sc_edges.len() == 1);
            let first_to_sc_edge = first_to_sc_edges[0].id();
            for arg in &mut function.graph_mut().edge_weight_mut(first_to_sc_edge).unwrap().arguments {
                if let Some(new_arg) = second_to_sc_args.iter().find(|(k, _)| k == &arg.0) {
                    *arg = new_arg.clone();
                }
            }

            let second_terminator = function.conditional_edges(pattern.second_node).unwrap();
            let other_edge = if second_terminator.0.target() == pattern.short_circuit {
                second_terminator.1
            } else {
                second_terminator.0
            };
            let other_edge = other_edge.id();
            assert!(skip_over_node(
                function,
                pattern.first_node,
                other_edge
            ));

            let mut removed_block = function.remove_block(pattern.second_node).unwrap();
            let first_node = pattern.first_node;
            if pattern.assign {
                let assign = removed_block
                    .first_mut()
                    .unwrap()
                    .as_assign_mut()
                    .unwrap();
                assign.right = vec![pattern.final_condition];
            } else {
                let removed_if = removed_block.last_mut().unwrap().as_if_mut().unwrap();
                removed_if.condition = pattern.final_condition;
            }
            if pattern.inverted {
                let removed_if = removed_block.last_mut().unwrap().as_if_mut().unwrap();
                // TODO: unnecessary clone?
                removed_if.condition = ast::Unary::new(removed_if.condition.clone(), UnaryOperation::Not).reduce();
            }
            let first_block = function.block_mut(first_node).unwrap();
            first_block.pop();
            first_block.extend(removed_block.0);
            did_structure = true;

            // crate::dot::render_to(function, &mut std::io::stdout()).unwrap();
        }
    }

    did_structure
}

// fn jumps_to_block_if_local(
//     function: &Function,
//     node: NodeIndex,
//     local: &ast::RcLocal,
//     target_node: NodeIndex,
// ) -> bool {
//     let block = function.block(node).unwrap();
//     // TODO: modify simplify_condition instead
//     // TODO: data flow analysis
//     /* local a = nil
//     print(a)
//     -- ...
//     if control ~= a */
//     let r#if = block[block.len() - 1].as_if().unwrap();
//     match r#if.condition.clone().reduce() {
//         ast::RValue::Binary(ast::Binary {
//             left: box ast::RValue::Local(cond_control),
//             right: box ast::RValue::Literal(ast::Literal::Nil),
//             operation,
//         }) if &cond_control == local => {
//             let (then_edge, else_edge) = block
//                 .terminator()
//                 .as_ref()
//                 .unwrap()
//                 .as_conditional()
//                 .unwrap();
//             match operation {
//                 ast::BinaryOperation::Equal => else_edge.node == target_node,
//                 ast::BinaryOperation::NotEqual => then_edge.node == target_node,
//                 _ => false,
//             }
//         }
//         ast::RValue::Local(cond_control) if &cond_control == local => {
//             block
//                 .terminator()
//                 .as_ref()
//                 .unwrap()
//                 .as_conditional()
//                 .unwrap()
//                 .0
//                 .node
//                 == target_node
//         }
//         ast::RValue::Unary(ast::Unary {
//             value: box ast::RValue::Local(cond_control),
//             operation: UnaryOperation::Not,
//         }) if &cond_control == local => {
//             block
//                 .terminator()
//                 .as_ref()
//                 .unwrap()
//                 .as_conditional()
//                 .unwrap()
//                 .1
//                 .node
//                 == target_node
//         }
//         _ => false,
//     }
// }

// fn match_for_next(
//     function: &Function,
//     post_dominators: &Dominators<NodeIndex>,
//     node: NodeIndex,
// ) -> Option<GenericForNextPattern> {
//     let block = function.block(node).unwrap();
//     if matches!(block.terminator, Some(Terminator::Conditional(..)))
//         && block.len() == 2
//         && let Some(assign) = block[block.len() - 2].as_assign()
//         && assign.right.len() == 1
//         && let assign_left = assign.left.iter().filter_map(|l| l.as_local().cloned()).collect::<Vec<_>>()
//         && assign_left.len() == assign.left.len()
//         && let Some(call) = assign.right[0].as_call()
//         && call.arguments.len() == 2
//         && let Some(state) = call.arguments[0].as_local().cloned()
//         && let Some(internal_control) = call.arguments[1].as_local().cloned()
//         && let Some(body_node) = function.successor_blocks(node).find(|&n| post_dominators.immediate_dominator(node) != Some(n))
//         && jumps_to_block_if_local(function, node, &assign_left[0], body_node)
//     {
//         Some(GenericForNextPattern {
//             body_node,
//             res_locals: assign_left,
//             generator: *call.value.to_owned(),
//             state,
//             internal_control
//         })
//     } else {
//         None
//     }
// }

/*
TODO: fix

function getLongestEntry(tab)
    local longest = 0
    for i,v in pairs(tab) do
        if string.len(v) > longest then
            longest = string.len(v)
        end
    end
    return longest
end
*/
pub fn structure_for_loops(
    function: &mut Function,
    dominators: &Dominators<NodeIndex>,
    post_dominators: &Dominators<NodeIndex>,
) -> bool {
    false

    // let mut did_structure = false;
    // for node in function.graph().node_indices().collect_vec() {
    //     if let Some(pattern) = match_for_next(function, post_dominators, node) {
    //         let mut init_blocks = function.predecessor_blocks(node).filter(|&n| {
    //             !dominators
    //                 .dominators(n)
    //                 .unwrap()
    //                 .contains(&pattern.body_node)
    //         });
    //         let init_node = init_blocks.next().unwrap();
    //         assert!(init_blocks.next().is_none());
    //         assert!(function.successor_blocks(init_node).count() == 1);

    //         let edges = function.edges_to_block(node);
    //         let params = edges[0]
    //             .1
    //             .arguments
    //             .iter()
    //             .map(|(p, _)| p)
    //             .collect::<FxHashSet<_>>();

    //         // TODO: this is a weird way to do it, should have stuff specifically for Luau and Lua 5.1 maybe?
    //         let mut generator_locals = pattern
    //             .generator
    //             .values_read()
    //             .into_iter()
    //             .filter(|l| !params.contains(l));
    //         if let Some(generator_local) = generator_locals.next()
    //             && generator_locals.next().is_none()
    //         {
    //             let generator_local = generator_local.clone();

    //             if params.contains(&pattern.internal_control) {
    //                 let initial_control = function.block(init_node).unwrap().terminator().as_ref().unwrap().edges()[0].arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1.clone();
    //                 let mut invalid_for = false;
    //                 for (_, edge) in edges.into_iter().filter(|(p, _)| p != &init_node) {
    //                     if edge.arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1
    //                         != pattern.res_locals[0] {
    //                             invalid_for = true;
    //                             break;
    //                         }
    //                 }
    //                 if !invalid_for {
    //                     for edge in function.edges_to_block_mut(node) {
    //                         edge.arguments.clear();
    //                     }

    //                     let block = function.block_mut(node).unwrap();
    //                     let len = block.ast.len();
    //                     block.ast.truncate(len - 2);
    //                     block.ast.push(
    //                         ast::GenericForNext::new(pattern.res_locals, pattern.generator, pattern.state.clone()).into()
    //                     );

    //                     function.block_mut(init_node).unwrap().ast.push(
    //                         ast::GenericForInit::new(generator_local.clone(), pattern.state, initial_control)
    //                             .into(),
    //                     );
    //                     did_structure = true;
    //                 }
    //             } else {
    //                 // initial_control is nil,
    //                 // there is only a single control variable
    //                 todo!();
    //             }
    //         }
    //     }
    // }
    // did_structure
}

fn match_method_call(call: &ast::Call) -> Option<(&ast::RValue, &str)> {
    // TODO: make sure `a:method with space()` doesnt happen
    if !call.arguments.is_empty()
        && !call.arguments[0].has_side_effects()
        && let Some(ast::Index { box left, right: box ast::RValue::Literal(ast::Literal::String(index)) }) = call.value.as_index()
        && left == &call.arguments[0]
    {
        if let Ok(index) = std::str::from_utf8(index) {
            Some((left, index))
        } else {
            None
        }
    } else {
        None
    }
}

// This code does not apply to Luau
pub fn structure_method_calls(function: &mut Function) -> bool {
    let mut did_structure = false;
    for (_, block) in function.blocks_mut() {
        for stat in &mut block.0 {
            if let ast::Statement::Call(call) = stat {
                if let Some((value, method)) = match_method_call(call) {
                    *stat = ast::MethodCall::new(
                        value.clone(),
                        method.to_string(),
                        call.arguments.drain(1..).collect(),
                    )
                    .into();
                    did_structure = true;
                }
            }
            stat.traverse_rvalues(&mut |rvalue| {
                if let ast::RValue::Call(call) = rvalue {
                    if let Some((value, method)) = match_method_call(call) {
                        *rvalue = ast::MethodCall::new(
                            value.clone(),
                            method.to_string(),
                            call.arguments.drain(1..).collect(),
                        )
                        .into();
                        did_structure = true;
                    }
                } else if let ast::RValue::Select(select) = rvalue {
                    if let ast::Select::Call(call) = select {
                        if let Some((value, method)) = match_method_call(call) {
                            *select = ast::MethodCall::new(
                                value.clone(),
                                method.to_string(),
                                call.arguments.drain(1..).collect(),
                            )
                            .into();
                            did_structure = true;
                        }
                    }
                }
            });
        }
    }
    did_structure
}

// TODO: STYLE: better argument names
// `before -> skip -> after` to `before -> after`.
// multiple `before -> skip` edges can exist.
// multiple `skip -> after` edges can exist, but we decide which to use based on
// the edge index
// updates edges
fn skip_over_node(
    function: &mut Function,
    before_node: NodeIndex,
    skip_to_after: EdgeIndex,
    // params from (skip_node, after_node)
    // parameters: &[(ast::RcLocal, ast::RValue)],
) -> bool {
    let (skip_node, after_node) = function.graph().edge_endpoints(skip_to_after).unwrap();
    let mut did_structure = false;
    let skip_to_after_args = function
        .graph()
        .edge_weight(skip_to_after)
        .unwrap()
        .arguments
        .clone();
    for edge in function
        .graph()
        .edges_directed(before_node, Direction::Outgoing)
        .filter(|e| e.target() == skip_node)
        .map(|e| e.id())
        .collect::<Vec<_>>()
    {
        let mut new_arguments = function
            .graph()
            .edge_weight(edge)
            .unwrap()
            .arguments
            .clone();
        new_arguments.extend(skip_to_after_args.iter().cloned());
        // TODO: eliminate duplicate arguments where possible

        // all arguments in edges to a block must have the same parameters
        // TODO: make arguments a map so order doesnt matter
        if !new_arguments
            .iter()
            .map(|(p, _)| p)
            .eq(skip_to_after_args.iter().map(|(p, _)| p))
        {
            continue;
        }

        let mut edge = function.graph_mut().remove_edge(edge).unwrap();
        edge.arguments = new_arguments.into_iter().collect();
        function.graph_mut().add_edge(before_node, after_node, edge);
        did_structure = true;
    }
    let block = function.block(before_node).unwrap();
    if !block.is_empty()
        && block.last().unwrap().as_if().is_some()
        && let Some((then_edge, else_edge)) = function.conditional_edges(skip_node)
        && then_edge.target() == else_edge.target()
        && then_edge.weight().arguments == else_edge.weight().arguments
    {
        // TODO: check if this works (+ restructuring/src/jump.rs)
        let cond = function
            .block_mut(before_node)
            .unwrap()
            .pop()
            .unwrap()
            .into_if()
            .unwrap()
            .condition;
        if cond.has_side_effects() {
            // TODO: assign not needed for calls (also see jump.rs)
            // well inline.rs should take care of them in this case, but in the case of jump.rs, not so much
            let temp_local = function.local_allocator.borrow_mut().allocate();
            function.block_mut(before_node).unwrap().push(
                ast::Assign {
                    left: vec![temp_local.into()],
                    right: vec![cond],
                    prefix: true,
                    parallel: false,
                }
                .into(),
            )
        }

        function.set_edges(
            before_node,
            vec![(after_node, BlockEdge::new(BranchType::Unconditional))],
        );
    }

    did_structure
}

// TODO: this code is repeated in LifterContext::lift
// TODO: rename to merge_blocks or something
pub fn structure_jumps(function: &mut Function, dominators: &Dominators<NodeIndex>) -> bool {
    let mut did_structure = false;
    for node in function.graph().node_indices().collect_vec() {
        // we call function.remove_block, that might've resulted in node being removed
        if function.block(node).is_some()
            && let Some(jump) = function.unconditional_edge(node)
            && jump.target() != node
        {
            let jump_target = jump.target();
            let jump_edge = jump.id();
            let block = function.block(node).unwrap();
            if block.is_empty() {
                let mut remove = true;
                for pred in function.predecessor_blocks(node).collect_vec() {
                    let did = skip_over_node(function, pred, jump_edge);
                    if did {
                        did_structure = true;
                    }
                    remove &= did;
                }
                if remove {
                    function.remove_block(node);
                if &Some(node) == function.entry() {
                    function.set_entry(jump_target);
                }
                }
            } else if function.predecessor_blocks(jump_target).count() == 1
                && dominators
                    .dominators(jump_target)
                    .map(|mut d| d.contains(&node))
                    .unwrap_or(false)
            {
                assert!(jump.weight().arguments.is_empty());
                let edges = function.remove_edges(jump_target);
                let body = function.remove_block(jump_target).unwrap();
                if &Some(jump_target) == function.entry() {
                    function.set_entry(node);
                }
                function.block_mut(node).unwrap().extend(body.0);
                function.set_edges(node, edges);
                did_structure = true;
            }
        }
    }
    did_structure
}
