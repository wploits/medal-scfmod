use cfg::{block::Terminator, dot, inline::inline_expressions};
use itertools::Itertools;
use petgraph::stable_graph::NodeIndex;

#[derive(Debug)]
struct CompoundAssignment {
    target: ast::RcLocal,
    value: ast::RValue,
}

impl super::GraphStructurer {
    // todo: there may be instructions unrelated to the compound conditional earlier in the block
    fn compound_info(&self, node: NodeIndex) -> Option<CompoundAssignment> {
        let mut statements = self.function.block(node).unwrap().ast.iter();
        if let Some(assign) = statements.next().unwrap().as_assign() {
            if assign.left.len() != 1 || assign.right.len() != 1 {
                return None;
            }
            let target = assign.left[0].0.clone();
            if let Ok(target) = target.into_local() {
                if let Some(statement) = statements.next() {
                    if let Some(if_stat) = statement.as_if() {
                        if let ast::RValue::Local(if_condition) = &*if_stat.condition {
                            if &target == if_condition {
                                return Some(CompoundAssignment {
                                    target,
                                    value: assign.right[0].clone(),
                                });
                            }
                        }
                    }
                    return None;
                }
                return Some(CompoundAssignment {
                    target,
                    value: assign.right[0].clone(),
                });
            }
        }
        None
    }

    fn simplify_condition(&mut self, node: NodeIndex) {
        let block = self.function.block_mut(node).unwrap();
        let if_stat = block.ast.last_mut().unwrap().as_if_mut().unwrap();
        if let Some(unary) = if_stat.condition.as_unary() {
            if_stat.condition = unary.value.clone();
            block.terminator.as_mut().unwrap().swap_edges();
        }
    }

    fn target_expression(
        &mut self,
        first_conditional: NodeIndex,
        second_conditional: NodeIndex,
        first_info: Option<CompoundAssignment>,
        second_info: Option<CompoundAssignment>,
    ) -> (&mut ast::RValue, ast::RValue) {
        let first_block = self.function.block(first_conditional).unwrap();
        let second_block = self.function.block(second_conditional).unwrap();
        if first_block.ast.len() == 2
            && second_block.ast.len() == 2
            && let Some(first_info) = first_info.as_ref()
            && let Some(second_info) = second_info.as_ref()
            && first_info.target == second_info.target
        {
            let assign = ast::Assign::new(
                vec![first_info.target.clone().into()],
                vec![first_info.target.clone().into()],
            );
            let block = self.function.block_mut(first_conditional).unwrap();
            block.ast.insert(1, assign.into());
            (
                &mut block
                    .ast
                    .iter_mut()
                    .rev()
                    .nth(1)
                    .unwrap()
                    .as_assign_mut()
                    .unwrap()
                    .right[0],
                second_info.value.clone(),
            )
        } else {
            let operand = *second_block
                .ast
                .last()
                .unwrap()
                .as_if()
                .unwrap()
                .condition
                .clone();
            (
                self.function
                    .block_mut(first_conditional)
                    .unwrap()
                    .ast
                    .last_mut()
                    .unwrap()
                    .as_if_mut()
                    .unwrap()
                    .condition
                    .as_mut(),
                operand,
            )
        }
    }

    fn combine_conditionals(
        &mut self,
        first_conditional: NodeIndex,
        second_conditional: NodeIndex,
        short_circuit: NodeIndex,
        end: NodeIndex,
    ) -> bool {
        self.simplify_condition(first_conditional);
        self.simplify_condition(second_conditional);

        let first_block = self.function.block(first_conditional).unwrap();
        let second_block = self.function.block(second_conditional).unwrap();

        if first_block.ast.len() > 2 || second_block.ast.len() > 2 {
            return false;
        }

        let first_terminator = first_block
            .terminator
            .as_ref()
            .unwrap()
            .as_conditional()
            .unwrap();
        let second_terminator = second_block
            .terminator
            .as_ref()
            .unwrap()
            .as_conditional()
            .unwrap();
        let (first_else, second_else) = (first_terminator.1.node, second_terminator.1.node);

        let first_info = self.compound_info(first_conditional);
        let second_info = self.compound_info(second_conditional);

        println!("{:#?} {:#?}", first_info, second_info);

        if first_info.is_none() && second_info.is_none() {
            return false;
        }

        let (target_expression, operand) = self.target_expression(
            first_conditional,
            second_conditional,
            first_info,
            second_info,
        );

        if first_else == second_conditional {
            if second_else == short_circuit {
                *target_expression = ast::Binary::new(
                    target_expression.clone(),
                    ast::Unary::new(operand, ast::UnaryOperation::Not).into(),
                    ast::BinaryOperation::Or,
                )
                .into();
            } else {
                *target_expression =
                    ast::Binary::new(target_expression.clone(), operand, ast::BinaryOperation::Or)
                        .into();
            }
        } else if second_else == short_circuit {
            *target_expression = ast::Binary::new(
                target_expression.clone(),
                operand,
                ast::BinaryOperation::And,
            )
            .into();
        } else {
            *target_expression = ast::Binary::new(
                target_expression.clone(),
                ast::Unary::new(operand, ast::UnaryOperation::Not).into(),
                ast::BinaryOperation::And,
            )
            .into();
        }

        //use ast::Reduce;
        //*target_expression = target_expression.clone().reduce();

        self.function
            .replace_edge(first_conditional, second_conditional, end);
        self.function.remove_block(second_conditional);

        true
    }

    pub fn match_and_or(&mut self, node: NodeIndex, assigner: NodeIndex, end: NodeIndex) -> bool {
        let info = self.compound_info(assigner);
        if info.is_none() {
            return false;
        }

        self.simplify_condition(node);

        let block = self.function.block(node).unwrap();
        let terminator = block.terminator.as_ref().unwrap().as_conditional().unwrap();
        let (then_node, _) = (terminator.0.node, terminator.1.node);

        let info = info.unwrap();

        let if_stat = block.ast.last().unwrap().as_if().unwrap();
        if if_stat.condition.as_local() != Some(&info.target) {
            return false;
        }

        let binary = ast::Binary::new(
            info.target.clone().into(),
            info.value.clone(),
            if then_node == assigner {
                ast::BinaryOperation::And
            } else {
                ast::BinaryOperation::Or
            },
        );
        let assign = ast::Assign::new(vec![info.target.into()], vec![binary.into()]);
        let block = self.function.block_mut(node).unwrap();
        block.ast.pop();
        block.ast.push(assign.into());

        self.function
            .set_block_terminator(node, Some(Terminator::jump(end)));
        self.function.remove_block(assigner);

        true
    }

    pub(crate) fn match_compound_conditional(
        &mut self,
        entry: NodeIndex,
        then_node: NodeIndex,
        else_node: NodeIndex,
    ) -> bool {
        if self.is_loop_header(then_node) || self.is_loop_header(else_node) {
            return false;
        }

        let else_successors = self.function.successor_blocks(else_node).collect_vec();
        let then_successors = self.function.successor_blocks(then_node).collect_vec();

        let mut changed = false;
        if else_node != entry
            && else_successors.contains(&then_node)
            && self.function.predecessor_blocks(else_node).count() == 1
        {
            if else_successors.len() == 2 {
                let end = *else_successors.iter().find(|&&n| n != then_node).unwrap();
                changed = self.combine_conditionals(entry, else_node, then_node, end);
            } else {
                changed = self.match_and_or(entry, else_node, then_node);
            }
        } else if then_node != entry
            && else_node != entry
            && then_successors.contains(&else_node)
            && self.function.predecessor_blocks(then_node).count() == 1
        {
            if then_successors.len() == 2 {
                let end = *then_successors.iter().find(|&&n| n != else_node).unwrap();
                changed = self.combine_conditionals(entry, then_node, else_node, end);
            } else {
                changed = self.match_and_or(entry, then_node, else_node);
            }
        }

        changed
    }
}
