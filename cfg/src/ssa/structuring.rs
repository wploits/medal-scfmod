use itertools::Itertools;
use petgraph::{stable_graph::NodeIndex, visit::DfsPostOrder, algo::dominators::Dominators};

use crate::{function::Function, block::{Terminator, BasicBlockEdge}, inline::inline_expressions};

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

fn simplify_condition(function: &mut Function, node: NodeIndex) {
    let block = function.block_mut(node).unwrap();
    if let Some(if_stat) = block.ast.last_mut().unwrap().as_if_mut()
        && let Some(unary) = if_stat.condition.as_unary() {
        if_stat.condition = *unary.value.clone();
        block.terminator.as_mut().unwrap().swap_edges();
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
    let successors = function.successor_blocks(node).collect_vec();
    if successors.len() != 2 {
        return None;
    }
    let r#if = function
        .block(node)
        .unwrap()
        .ast
        .last()
        .unwrap()
        .as_if()
        .unwrap();

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
            if function.successor_blocks(assigner).collect_vec() == vec![next]
                && function.predecessor_blocks(assigner).collect_vec() == vec![node]
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
                operator: PatternOperator::And
            });
        } else if let Some((assigned_local, assigned_value, parameter)) = test_pattern(b, a) {
            return Some(ConditionalAssignmentPattern {
                assigner: b,
                next: a,
                tested_local: condition.clone(),
                assigned_local,
                assigned_value,
                parameter,
                operator: PatternOperator::And
            });
        }
    }

    None
}

pub fn structure_compound_conditionals(function: &mut Function) {
    let mut dfs = DfsPostOrder::new(function.graph(), function.entry().unwrap());
    while let Some(node) = dfs.next(function.graph()) {
        simplify_condition(function, node);
        if let Some(pattern) = match_conditional_assignment(function, node) {
            let block = function.block_mut(node).unwrap();
            block.ast.pop();
            block.ast.push(ast::Assign::new(vec![pattern.assigned_local.clone().into()], vec![ast::Binary::new(pattern.tested_local.into(), pattern.assigned_value, pattern.operator.into()).into()]).into());

            let edges = block.terminator.take().unwrap().into_conditional().unwrap();
            let mut arguments = if edges.0.node == pattern.next { edges.0.arguments } else { edges.1.arguments };
            for (parameter, argument) in arguments.iter_mut() {
                if *parameter == pattern.parameter {
                    *argument = pattern.assigned_local;
                    break;
                }
            }
            function.remove_block(pattern.assigner);
            function.set_block_terminator(node, Some(Terminator::Jump(BasicBlockEdge { node: pattern.next, arguments })));

            inline_expressions(function);
        }
    }
}

pub fn structure_jumps(function: &mut Function, dominators: &Dominators<NodeIndex>) {
    let mut dfs = DfsPostOrder::new(function.graph(), function.entry().unwrap());
    while let Some(node) = dfs.next(function.graph()) {
        if let Some(Terminator::Jump(jump)) = function.block(node).unwrap().terminator.clone()
            && jump.arguments.is_empty()
            && function.predecessor_blocks(jump.node).collect_vec() == vec![node]
            && dominators.dominators(jump.node).unwrap().contains(&node)
        {
            let terminator = function.block_mut(jump.node).unwrap().terminator.take();
            let body = function.remove_block(jump.node).unwrap().ast;
            function.block_mut(node).unwrap().ast.extend(body.0);
            function.set_block_terminator(node, terminator);
        }
    }
}