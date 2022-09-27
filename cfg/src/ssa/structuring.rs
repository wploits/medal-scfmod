use ast::LocalRw;
use ast::{Reduce, UnaryOperation};
use fxhash::FxHashSet;
use itertools::Itertools;
use petgraph::{
    algo::dominators::Dominators,
    stable_graph::NodeIndex,
    visit::{DfsPostOrder},
};

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
                {
                    let assign_edge = function.block(assigner).unwrap().terminator.as_ref().unwrap().as_jump().unwrap();
                    let other_edge = if edges.0.node == next { edges.0 } else { edges.1 };
                    
                    let assign_parameter = assign_edge.arguments.iter().find_map(|(k, v)| if v == assigned_local { Some(k) } else { None }).unwrap();
                    let other_parameter = other_edge.arguments.iter().find_map(|(k, v)| if v == condition { Some(k) } else { None }).unwrap();
    
                    if assign_parameter == other_parameter {
                        return Some((assigned_local.clone(), assign.right[0].clone(), assign_parameter.clone()));
                    }
                }
                None
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

pub fn structure_for_loops(
    function: &mut Function,
    dominators: &Dominators<NodeIndex>,
    post_dominators: &Dominators<NodeIndex>,
) -> bool {
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

// TODO: this code is repeated in LifterContext::lift
// TODO: rename to merge_blocks or something
pub fn structure_jumps(function: &mut Function, dominators: &Dominators<NodeIndex>) -> bool {
    let mut did_structure = false;
    for node in function.graph().node_indices().collect_vec() {
        // we call function.remove_block, that might've resulted in node being removed
        if let Some(block) = function.block(node) {
            if let Some(Terminator::Jump(jump)) = block.terminator.clone()
                && jump.node != node
                && jump.arguments.is_empty()
                && function.predecessor_blocks(jump.node).collect_vec() == [node]
                && dominators.dominators(jump.node).map(|mut d| d.contains(&node)).unwrap_or(false)
            {
                let terminator = function.block_mut(jump.node).unwrap().terminator.take();
                let body = function.remove_block(jump.node).unwrap().ast;
                function.block_mut(node).unwrap().ast.extend(body.0);
                function.set_block_terminator(node, terminator);
                did_structure = true;
            }
        }
    }
    did_structure
}
