use array_tool::vec::Intersect;
use ast::{LocalRw, Reduce, SideEffects, Traverse, UnaryOperation};

use itertools::Itertools;
use petgraph::{
    algo::dominators::Dominators,
    stable_graph::{EdgeIndex, NodeIndex},
    visit::{DfsPostOrder, EdgeRef},
    Direction,
};
use rustc_hash::{FxHashMap, FxHashSet};
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
        let test_pattern = |second_conditional, other, other_args: FxHashMap<_, _>| {
            let second_conditional_successors = function.edges(second_conditional).collect_vec();
            let second_block = function.block(second_conditional).unwrap();
            if let Some(second_conditional_if) = second_block.last().and_then(|s| s.as_if()) {
                if second_conditional_successors.len() == 2
                    && let Ok(edge_to_other) = second_conditional_successors
                        .iter()
                        .filter(|&s| s.target() == other && s.weight().arguments.iter().all(|(p, _)| other_args.contains_key(p)))
                        .exactly_one()
                {
                    if second_block.len() == 2 {
                        if let ast::Statement::Assign(assign) = &second_block[0] {
                            // TODO: make sure this variable isnt used anywhere but this block
                            // and the args passed to other.
                            let values_written = assign.values_written();
                            if values_written.len() == 1
                                && second_conditional_if.condition
                                    == values_written[0].clone().into()
                            {
                                let valid = if other_args.len() == 1 && let Ok((_, ast::RValue::Local(local))) = edge_to_other.weight().arguments.iter().exactly_one()
                                && local == values_written[0] {
                                    true
                                } else { other_args.is_empty() };
                                if valid {
                                    assert!(assign.right.len() == 1);
                                    return Some((assign.right[0].clone(), true));
                                }
                            }
                        }
                        return None;
                    } else if second_block.len() == 1
                        // TODO: check if all arguments the same instead
                        && edge_to_other.weight().arguments.is_empty()
                    {
                        return Some((second_conditional_if.condition.clone(), false));
                    }
                }
            }
            None
        };
        let first_terminator = function.conditional_edges(node).unwrap();
        let (then_edge, else_edge) = first_terminator;
        if function.predecessor_blocks(then_edge.target()).count() == 1
            && then_edge.weight().arguments.is_empty()
            && let else_args = else_edge.weight().arguments.iter().cloned().collect::<FxHashMap<_, _>>()
            && let Some((second_condition, assign)) = test_pattern(then_edge.target(), else_edge.target(), else_args)
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
            && else_edge.weight().arguments.is_empty()
            && let then_args = then_edge.weight().arguments.iter().cloned().collect::<FxHashMap<_, _>>()
            && let Some((second_condition, assign)) = test_pattern(else_edge.target(), then_edge.target(), then_args)
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

            let (a, b) = edges.map(|e| e.target());
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

pub fn structure_conditionals(function: &mut Function) -> bool {
    let mut did_structure = false;
    // TODO: does this need to be in dfs post order?
    let mut dfs = DfsPostOrder::new(function.graph(), function.entry().unwrap());
    while let Some(node) = dfs.next(function.graph()) {
        if simplify_condition(function, node) {
            did_structure = true;
        }
        if structure_bool_conditional(function, node) {
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
        }

        did_structure |= try_remove_unnecessary_condition(function, node);
    }

    did_structure
}

// TODO: REFACTOR: move to ast
fn is_truthy(rvalue: ast::RValue) -> bool {
    match rvalue.reduce() {
        // __len has to return number, but __unm can return any value
        ast::RValue::Unary(ast::Unary {
            operation: ast::UnaryOperation::Length,
            ..
        }) => true,
        ast::RValue::Literal(
            ast::Literal::Boolean(true) | ast::Literal::Number(_) | ast::Literal::String(_),
        )
        | ast::RValue::Table(_)
        | ast::RValue::Closure(_) => true,
        _ => false,
    }
}

// TODO: STYLE: rename
fn make_bool_conditional(
    function: &mut Function,
    node: NodeIndex,
    mut then_value: ast::RValue,
    mut else_value: ast::RValue,
) -> Option<ast::RValue> {
    let block = function.block_mut(node).unwrap();
    let r#if = block.last_mut().unwrap().as_if_mut().unwrap();
    if let ast::RValue::Literal(ast::Literal::Boolean(then_value)) = then_value
        && let ast::RValue::Literal(ast::Literal::Boolean(else_value)) = else_value
        && then_value != else_value
    {
        let cond: ast::RValue = match std::mem::replace(&mut r#if.condition, ast::Literal::Nil.into()).reduce() {
            ast::RValue::Binary(binary) if binary.operation.is_comparator() => match then_value {
                true => binary.into(),
                false => ast::Unary::new(binary.into(), ast::UnaryOperation::Not).into(),
            },
            ast::RValue::Literal(ast::Literal::Boolean(bool)) => ast::Literal::Boolean(bool == then_value).into(),
            cond => match then_value {
                true => ast::Binary::new(ast::Binary::new(cond, ast::Literal::Boolean(true).into(), ast::BinaryOperation::And).into(), ast::Literal::Boolean(false).into(), ast::BinaryOperation::Or).into(),
                false => ast::Unary::new(cond, ast::UnaryOperation::Not).into(),
            },
        };
        Some(cond.reduce())
    } else {
        let then_truthy = is_truthy(then_value.clone());
        let else_truthy = is_truthy(else_value.clone());
        let cond = if !then_truthy && !else_truthy {
            return None
        } else if !then_truthy {
            std::mem::swap(&mut then_value, &mut else_value);
            ast::Unary::new(std::mem::replace(&mut r#if.condition, ast::Literal::Nil.into()), ast::UnaryOperation::Not).reduce()
        } else if !else_truthy {
            std::mem::replace(&mut r#if.condition, ast::Literal::Nil.into()).reduce()
        } else {
            let cond = std::mem::replace(&mut r#if.condition, ast::Literal::Nil.into()).reduce();
            if let ast::RValue::Unary(ast::Unary { box value, operation: ast::UnaryOperation::Not }) = cond {
                std::mem::swap(&mut then_value, &mut else_value);
                value
            } else {
                cond
            }
        };

        Some(ast::Binary::new(ast::Binary::new(cond, then_value, ast::BinaryOperation::And).into(), else_value, ast::BinaryOperation::Or).into())
    }
}

// TODO: `return if g then true else false` in luau?
// local a; if g then a = true else a = false end; return a -> return g and true or false
// local a; if g then a = false else a = true end; return a -> return not g
// local a; if g == 1 then a = true else a = false end; return a -> return g == 1
fn structure_bool_conditional(function: &mut Function, node: NodeIndex) -> bool {
    if let Some(ast::Statement::If(_)) = function.block(node).unwrap().last() {
        let (then_edge, else_edge) = function.conditional_edges(node).unwrap();
        if then_edge.target() == else_edge.target()
            && let Ok((res_local, then_value, else_value)) = then_edge.weight().arguments.iter().filter_map(|(p, a)| else_edge.weight().arguments.iter().find(|(p1, _)| p == p1).map(|(_, a1)| (p, a, a1))).into_iter().exactly_one()
        {
            let (then_edge, else_edge) = (then_edge.id(), else_edge.id());
            let res_local = res_local.clone();
            let then_value = then_value.clone();
            let else_value = else_value.clone();

            if let Some(res) = make_bool_conditional(function, node, then_value, else_value) {
                function.graph_mut().edge_weight_mut(then_edge).unwrap().arguments[0].1 = res_local.clone().into();
                function.graph_mut().edge_weight_mut(else_edge).unwrap().arguments[0].1 = res_local.clone().into();
                let block = function.block_mut(node).unwrap();
                let r#if = block.last_mut().unwrap().as_if_mut().unwrap();
                r#if.condition = res_local.clone().into();
                let pos = block.len() - 1;
                block.insert(pos, ast::Assign::new(vec![res_local.into()], vec![res]).into());
                true
            } else {
                false
            }
        } else if then_edge.target() != else_edge.target()
            && let (then_target, else_target) = (then_edge.target(), else_edge.target())
            && function.predecessor_blocks(then_target).exactly_one().is_ok() && function.predecessor_blocks(else_target).exactly_one().is_ok()
            && function.successor_blocks(then_target).next().is_none() && function.successor_blocks(else_target).next().is_none()
            && let Ok(ast::Statement::Return(ast::Return { values: then_values })) = function.block(then_target).unwrap().iter().exactly_one()
            && let Ok(then_value) = then_values.iter().exactly_one()
            && let Ok(ast::Statement::Return(ast::Return { values: else_values })) = function.block(else_target).unwrap().iter().exactly_one()
            && let Ok(else_value) = else_values.iter().exactly_one()
        {
            let then_value = then_value.clone();
            let else_value = else_value.clone();

            if let Some(res) = make_bool_conditional(function, node, then_value, else_value) {
                function.remove_block(then_target);
                function.remove_block(else_target);
                let block = function.block_mut(node).unwrap();
                block.pop();
                block.push(ast::Return::new(vec![res]).into());
                true
            } else {
                false
            }
        }
        
        else {
            false
        }
    } else {
        false
    }
    //todo!();
}

fn jumps_to_block_if_local(
    function: &Function,
    node: NodeIndex,
    local: &ast::RcLocal,
    target_node: NodeIndex,
) -> bool {
    let block = function.block(node).unwrap();
    // TODO: modify simplify_condition instead
    // TODO: data flow analysis
    /* local a = nil
    print(a)
    -- ...
    if control ~= a */
    let r#if = block[block.len() - 1].as_if().unwrap();
    match r#if.condition.clone().reduce() {
        ast::RValue::Binary(ast::Binary {
            left: box ast::RValue::Local(cond_control),
            right: box ast::RValue::Literal(ast::Literal::Nil),
            operation,
        }) if &cond_control == local => {
            let (then_edge, else_edge) = function.conditional_edges(node).unwrap();
            match operation {
                ast::BinaryOperation::Equal => else_edge.target() == target_node,
                ast::BinaryOperation::NotEqual => then_edge.target() == target_node,
                _ => false,
            }
        }
        // TODO: does uncommenting this result in semantically correct code
        // for generic for loops? see:
        // for i, v in function() return false, 2 end do print(i, v) end
        // for i, v in function() return nil, 2 end do print(i, v) end

        // ast::RValue::Local(cond_control) if &cond_control == local => {
        //     let (then_edge, _) = function.conditional_edges(node).unwrap();
        //     then_edge.target() == target_node
        // }
        // ast::RValue::Unary(ast::Unary {
        //     value: box ast::RValue::Local(cond_control),
        //     operation: UnaryOperation::Not,
        // }) if &cond_control == local => {
        //     let (_, else_edge) = function.conditional_edges(node).unwrap();
        //     else_edge.target() == target_node
        // }
        _ => false,
    }
}

fn match_for_next(
    function: &Function,
    post_dominators: &Dominators<NodeIndex>,
    node: NodeIndex,
) -> Option<GenericForNextPattern> {
    if function.conditional_edges(node).is_some()
        && let block = function.block(node).unwrap()
        && block.len() == 2
        && let Some(assign) = block[block.len() - 2].as_assign()
        && assign.right.len() == 1
        && let assign_left = assign.left.iter().filter_map(|l| l.as_local().cloned()).collect::<Vec<_>>()
        && assign_left.len() == assign.left.len()
        && let Some(call) = assign.right[0].as_call()
        && call.arguments.len() == 2
        && let Some(state) = call.arguments[0].as_local().cloned()
        && let Some(internal_control) = call.arguments[1].as_local().cloned()
        && let Some(body_node) = function.successor_blocks(node).find(|&n| post_dominators.immediate_dominator(node) != Some(n))
        && jumps_to_block_if_local(function, node, &assign_left[0], body_node)
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

            let edges = function
                .graph()
                .edges_directed(node, Direction::Incoming)
                .collect::<Vec<_>>();
            let params = edges[0]
                .weight()
                .arguments
                .iter()
                .map(|(p, _)| p.clone())
                .collect::<Vec<_>>();

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
                    let mut invalid_for = false;
                    for edge in edges.iter().filter(|p| p.source() != init_node) {
                        if edge.weight().arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1.as_local().unwrap()
                            != &pattern.res_locals[0] {
                                invalid_for = true;
                                break;
                            }
                    }
                    if !invalid_for {
                        let initial_control = function.unconditional_edge(init_node).unwrap().weight().arguments.iter().find(|(p, _)| p == &pattern.internal_control).unwrap().1.as_local().unwrap().clone();
                        let edges = edges.into_iter().map(|e| e.id()).collect::<Vec<_>>();
                        for edge in edges {
                            function.graph_mut().edge_weight_mut(edge).unwrap().arguments.retain(|(p, _)| p != &pattern.internal_control);
                        }

                        let block = function.block_mut(node).unwrap();
                        let len = block.len();
                        block.truncate(len - 2);
                        block.push(
                            ast::GenericForNext::new(pattern.res_locals, pattern.generator, pattern.state.clone()).into()
                        );

                        function.block_mut(init_node).unwrap().push(
                            ast::GenericForInit::new(generator_local.clone(), pattern.state, initial_control)
                                .into(),
                        );
                        did_structure = true;
                    }
                } else {
                    // TODO: initial_control is nil,
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

    did_structure
}

fn try_remove_unnecessary_condition(function: &mut Function, node: NodeIndex) -> bool {
    let block = function.block(node).unwrap();
    if !block.is_empty()
        && block.last().unwrap().as_if().is_some()
        && let Some((then_edge, else_edge)) = function.conditional_edges(node)
        && then_edge.target() == else_edge.target()
        && then_edge.weight().arguments == else_edge.weight().arguments
    {
        let target = then_edge.target();
        // TODO: check if this works (+ restructuring/src/jump.rs)
        let cond = function
            .block_mut(node)
            .unwrap()
            .pop()
            .unwrap()
            .into_if()
            .unwrap()
            .condition;
        let new_stat = match cond {
            ast::RValue::Call(call) => Some(call.into()),
            ast::RValue::MethodCall(method_call) => Some(method_call.into()),
            cond if cond.has_side_effects() => {
                let temp_local = function.local_allocator.borrow_mut().allocate();
                Some(ast::Assign {
                        left: vec![temp_local.into()],
                        right: vec![cond],
                        prefix: true,
                        parallel: false,
                    }
                    .into(),
                )
            },
            _ => None,
        };
        function.block_mut(node).unwrap().extend(new_stat);
        let arguments = function.remove_edges(node).into_iter().next().unwrap().1.arguments;
        let mut new_edge = BlockEdge::new(BranchType::Unconditional);
        new_edge.arguments = arguments;
        function.set_edges(
            node,
            vec![(target, new_edge)],
        );
        true
    } else {
        false
    }
}

// TODO: REFACTOR: same as match_jump in restructure, maybe can use some common code?
// TODO: STYLE: rename to merge_blocks or something
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
            // TODO: block_is_no_op?
            if block.is_empty() {
                let mut remove = true;
                for pred in function.predecessor_blocks(node).collect_vec() {
                    let did = skip_over_node(function, pred, jump_edge) | try_remove_unnecessary_condition(function, pred);
                    if did {
                        did_structure = true;
                    }
                    remove &= did;
                }
                if remove && function.entry() != &Some(node) {
                    function.remove_block(node);
                    continue;
                }
            }
            if function.predecessor_blocks(jump_target).count() == 1
                && dominators
                    .dominators(jump_target)
                    .map(|mut d| d.contains(&node))
                    .unwrap_or(false)
            {
                assert!(function.graph().edge_weight(jump_edge).unwrap().arguments.is_empty());
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
