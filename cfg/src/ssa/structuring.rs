use ast::{LocalRw, SideEffects, Traverse};
use ast::{Reduce, UnaryOperation};
use fxhash::FxHashSet;
use itertools::Itertools;
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::DfsPostOrder};

use crate::dot::render_to;
use crate::{
    block::{BasicBlock, BasicBlockEdge, Terminator},
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
    if let Some(if_stat) = block.ast.last_mut().and_then(|s| s.as_if_mut())
        && let Some(unary) = if_stat.condition.as_unary() {
        if_stat.condition = *unary.value.clone();
        block.terminator.as_mut().unwrap().swap_edges();
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
    let block = function.block(node).unwrap();
    if let Some(r#if) = block.ast.last().and_then(|s| s.as_if()) {
        let first_condition = r#if.condition.clone();
        let test_pattern = |second_conditional, other| {
            let second_conditional_successors =
                function.successor_blocks(second_conditional).collect_vec();
            let second_block = function.block(second_conditional).unwrap();
            if let Some(second_conditional_if) = second_block.ast.last().and_then(|s| s.as_if()) {
                if second_conditional_successors.len() == 2
                    && second_conditional_successors.contains(&other)
                {
                    if second_block.ast.len() == 2 {
                        if let ast::Statement::Assign(assign) = &second_block.ast[0] {
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
                    }
                    return Some((second_conditional_if.condition.clone(), false));
                }
            }
            None
        };
        let first_terminator = block.terminator.as_ref().unwrap().as_conditional().unwrap();
        let (then_edge, else_edge) = block.terminator.as_ref().unwrap().as_conditional().unwrap();
        if function.predecessor_blocks(then_edge.node).count() == 1 && let Some((second_condition, assign)) = test_pattern(then_edge.node, else_edge.node) {
            let second_terminator = function.block(then_edge.node).unwrap().terminator.as_ref().unwrap().as_conditional().unwrap();
            if second_terminator.0.node == else_edge.node {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: then_edge.node,
                    short_circuit: else_edge.node,
                    assign,
                    final_condition: ast::Binary::new(first_condition, ast::Unary::new(second_condition, ast::UnaryOperation::Not).into(), ast::BinaryOperation::And).into()
                })
            } else {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: then_edge.node,
                    short_circuit: else_edge.node,
                    assign,
                    final_condition: ast::Binary::new(first_condition, second_condition, ast::BinaryOperation::And).into()
                })
            }
        } else if function.predecessor_blocks(else_edge.node).count() == 1 && let Some((second_condition, assign)) = test_pattern(else_edge.node, then_edge.node) {
            let second_terminator = function.block(else_edge.node).unwrap().terminator.as_ref().unwrap().as_conditional().unwrap();
            if first_terminator.0.node == second_terminator.0.node {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: else_edge.node,
                    short_circuit: then_edge.node,
                    assign,
                    final_condition: ast::Binary::new(first_condition, second_condition, ast::BinaryOperation::Or).into()
                })
            } else {
                Some(ConditionalSequencePattern {
                    first_node: node,
                    second_node: else_edge.node,
                    short_circuit: then_edge.node,
                    assign,
                    final_condition: ast::Binary::new(first_condition, ast::Unary::new(second_condition, ast::UnaryOperation::Not).into(), ast::BinaryOperation::Or).into()
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
    if let Some(r#if) = function
        .block(node)
        .unwrap()
        .ast
        .last()
        .and_then(|s| s.as_if())
    {
        let edges = function
            .block(node)
            .unwrap()
            .terminator
            .as_ref()
            .unwrap()
            .as_conditional()
            .unwrap();

        if let ast::RValue::Local(condition) = &r#if.condition {
            let test_pattern = |assigner, next| {
                if function.successor_blocks(assigner).collect_vec() == [next]
                    && function.predecessor_blocks(assigner).collect_vec() == [node]
                    && let Some(assign) = single_assign(&function.block(assigner).unwrap().ast)
                    && assign.left.len() == 1 && assign.right.len() == 1
                    && let ast::LValue::Local(assigned_local) = &assign.left[0]
                    && let Some(assigner_terminator) = function.block(assigner).unwrap().terminator.as_ref()
                    && let Some(assign_edge) = assigner_terminator.as_jump()
                    && let other_edge = if edges.0.node == next { edges.0 } else { edges.1 }
                    && let Some(assign_param) = assign_edge.arguments.iter().find_map(|(k, v)| if v == assigned_local { Some(k) } else { None })
                    && let Some(other_param) = other_edge.arguments.iter().find_map(|(k, v)| if v == condition { Some(k) } else { None })
                    && assign_param == other_param
                {
                    Some((assigned_local.clone(), assign.right[0].clone(), assign_param.clone()))
                } else {
                    None
                }
            };

            let (a, b) = (edges.0.node, edges.1.node);
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
        if let Some(pattern) = match_conditional_assignment(function, node) {
            let block = function.block_mut(node).unwrap();
            block.ast.pop();
            block.ast.push(
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

            let edges = block.terminator.take().unwrap().into_conditional().unwrap();
            let mut arguments = if edges.0.node == pattern.next {
                edges.0.arguments
            } else {
                edges.1.arguments
            };
            for (parameter, argument) in arguments.iter_mut() {
                if *parameter == pattern.parameter {
                    *argument = pattern.assigned_local;
                    break;
                }
            }
            function.remove_block(pattern.assigner);
            function.set_block_terminator(
                node,
                Some(Terminator::Jump(BasicBlockEdge {
                    node: pattern.next,
                    arguments,
                })),
            );

            did_structure = true;
        }

        if let Some(pattern) = match_conditional_sequence(function, node) {
            let edges = function
                .block(pattern.first_node)
                .unwrap()
                .terminator
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap();
            let (first_then, first_else) = (edges.0.node, edges.1.node);

            let second_terminator = function
                .block(pattern.second_node)
                .unwrap()
                .terminator
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap();
            let other_edge = if second_terminator.0.node == pattern.short_circuit {
                second_terminator.1
            } else {
                second_terminator.0
            };
            replace_edge_with_parameters(
                function,
                pattern.first_node,
                pattern.second_node,
                other_edge.node,
                other_edge.arguments.clone(),
            );

            let mut removed_block = function.remove_block(pattern.second_node).unwrap();
            let first_node = pattern.first_node;
            if pattern.assign {
                let assign = removed_block
                    .ast
                    .first_mut()
                    .unwrap()
                    .as_assign_mut()
                    .unwrap();
                assign.right = vec![pattern.final_condition];
            } else {
                let removed_if = removed_block.ast.last_mut().unwrap().as_if_mut().unwrap();
                removed_if.condition = pattern.final_condition;
            }
            let first_block = function.block_mut(first_node).unwrap();
            first_block.ast.pop();
            first_block.ast.extend(removed_block.ast.0);
            did_structure = true;

            // crate::dot::render_to(function, &mut std::io::stdout()).unwrap();
        }
    }

    did_structure
}

fn jumps_to_block_if_local(
    block: &BasicBlock,
    local: &ast::RcLocal,
    target_node: NodeIndex,
) -> bool {
    // TODO: modify simplify_condition instead
    // TODO: data flow analysis
    /* local a = nil
    print(a)
    -- ...
    if control ~= a */
    let r#if = block.ast[block.ast.len() - 1].as_if().unwrap();
    match r#if.condition.clone().reduce() {
        ast::RValue::Binary(ast::Binary {
            left: box ast::RValue::Local(cond_control),
            right: box ast::RValue::Literal(ast::Literal::Nil),
            operation,
        }) if &cond_control == local => {
            let (then_edge, else_edge) = block
                .terminator()
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap();
            match operation {
                ast::BinaryOperation::Equal => else_edge.node == target_node,
                ast::BinaryOperation::NotEqual => then_edge.node == target_node,
                _ => false,
            }
        }
        ast::RValue::Local(cond_control) if &cond_control == local => {
            block
                .terminator()
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap()
                .0
                .node
                == target_node
        }
        ast::RValue::Unary(ast::Unary {
            value: box ast::RValue::Local(cond_control),
            operation: UnaryOperation::Not,
        }) if &cond_control == local => {
            block
                .terminator()
                .as_ref()
                .unwrap()
                .as_conditional()
                .unwrap()
                .1
                .node
                == target_node
        }
        _ => false,
    }
}

fn match_for_next(
    function: &Function,
    post_dominators: &Dominators<NodeIndex>,
    node: NodeIndex,
) -> Option<GenericForNextPattern> {
    let block = function.block(node).unwrap();
    if matches!(block.terminator, Some(Terminator::Conditional(..)))
        && block.ast.len() == 2
        && let Some(assign) = block.ast[block.ast.len() - 2].as_assign()
        && assign.right.len() == 1
        && let assign_left = assign.left.iter().filter_map(|l| l.as_local().cloned()).collect::<Vec<_>>()
        && assign_left.len() == assign.left.len()
        && let Some(call) = assign.right[0].as_call()
        && call.arguments.len() == 2
        && let Some(state) = call.arguments[0].as_local().cloned()
        && let Some(internal_control) = call.arguments[1].as_local().cloned()
        && let Some(body_node) = function.successor_blocks(node).find(|&n| post_dominators.immediate_dominator(node) != Some(n))
        && jumps_to_block_if_local(block, &assign_left[0], body_node)
    {
        Some(GenericForNextPattern {
            body_node,
            res_locals: assign_left,
            generator: *call.value.to_owned(),
            state,
            internal_control
        })
    } else {
        None
    }
}

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
    return false;

    let mut did_structure = false;
    for node in function.graph().node_indices().collect_vec() {
        if let Some(pattern) = match_for_next(function, post_dominators, node) {
            let mut init_blocks = function.predecessor_blocks(node).filter(|&n| {
                !dominators
                    .dominators(n)
                    .unwrap()
                    .contains(&pattern.body_node)
            });
            let init_node = init_blocks.next().unwrap();
            assert!(init_blocks.next().is_none());
            assert!(function.successor_blocks(init_node).count() == 1);

            let edges = function.edges_to_block(node);
            let params = edges[0]
                .1
                .arguments
                .iter()
                .map(|(p, _)| p)
                .collect::<FxHashSet<_>>();

            // TODO: this is a weird way to do it, should have stuff specifically for Luau and Lua 5.1 maybe?
            let mut generator_locals = pattern
                .generator
                .values_read()
                .into_iter()
                .filter(|l| !params.contains(l));
            if let Some(generator_local) = generator_locals.next()
                && generator_locals.next().is_none()
            {
                let generator_local = generator_local.clone();

                if params.contains(&pattern.internal_control) {
                    let initial_control = function.block(init_node).unwrap().terminator().as_ref().unwrap().edges()[0].arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1.clone();
                    let mut invalid_for = false;
                    for (_, edge) in edges.into_iter().filter(|(p, _)| p != &init_node) {
                        if edge.arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1
                            != pattern.res_locals[0] {
                                invalid_for = true;
                                break;
                            }
                    }
                    if !invalid_for {
                        for edge in function.edges_to_block_mut(node) {
                            edge.arguments.clear();
                        }

                        let block = function.block_mut(node).unwrap();
                        let len = block.ast.len();
                        block.ast.truncate(len - 2);
                        block.ast.push(
                            ast::GenericForNext::new(pattern.res_locals, pattern.generator, pattern.state.clone()).into()
                        );
        
                        function.block_mut(init_node).unwrap().ast.push(
                            ast::GenericForInit::new(generator_local.clone(), pattern.state, initial_control)
                                .into(),
                        );
                        did_structure = true;
                    }
                } else {
                    // initial_control is nil,
                    // there is only a single control variable
                    todo!();
                }
            }
        }
    }
    did_structure
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
        for stat in &mut block.ast.0 {
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

fn replace_edge_with_parameters(
    function: &mut Function,
    node: NodeIndex,
    old_target: NodeIndex,
    new_target: NodeIndex,
    parameters: Vec<(ast::RcLocal, ast::RcLocal)>,
) {
    match function
        .block_mut(node)
        .unwrap()
        .terminator
        .as_mut()
        .unwrap()
    {
        Terminator::Jump(jump) => {
            if jump.node == old_target {
                jump.arguments.extend(parameters);
            }
        }
        Terminator::Conditional(then_edge, else_edge) => {
            if then_edge.node == old_target {
                then_edge.arguments.extend(parameters);
            } else if else_edge.node == old_target {
                else_edge.arguments.extend(parameters);
            }
        }
    }
    function.replace_edge(node, old_target, new_target);
}

// TODO: this code is repeated in LifterContext::lift
// TODO: rename to merge_blocks or something
pub fn structure_jumps(function: &mut Function, dominators: &Dominators<NodeIndex>) -> bool {
    let mut did_structure = false;
    for node in function.graph().node_indices().collect_vec() {
        // we call function.remove_block, that might've resulted in node being removed
        if let Some(block) = function.block(node) {
            if let Some(Terminator::Jump(jump)) = block.terminator.clone()
                && jump.node != node
            {
                if block.ast.is_empty() {
                    let old_terminator = function.block(node).unwrap().terminator.as_ref().unwrap().as_jump().unwrap().clone();
                    let mut can_remove = true;
                    for pred in function.predecessor_blocks(node).collect_vec() {
                        // TODO: support multiple edges from pred > jump.node
                        if function.successor_blocks(pred).any(|s| s == jump.node) {
                            can_remove = false;
                        } else {
                            replace_edge_with_parameters(function, pred, node, jump.node, old_terminator.arguments.clone());
                        }
                    }
                    if can_remove {
                        function.remove_block(node);
                    }
                    did_structure = true;
                } else if function.predecessor_blocks(jump.node).count() == 1
                    && dominators.dominators(jump.node).map(|mut d| d.contains(&node)).unwrap_or(false)
                {
                    assert!(jump.arguments.is_empty());
                    let terminator = function.block_mut(jump.node).unwrap().terminator.take();
                    let body = function.remove_block(jump.node).unwrap().ast;
                    function.block_mut(node).unwrap().ast.extend(body.0);
                    function.set_block_terminator(node, terminator);
                    did_structure = true;
                }
            }
        }
    }
    did_structure
}
