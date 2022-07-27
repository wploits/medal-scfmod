use cfg::{block::Terminator, dot};
use graph::NodeId;

struct CompoundAssignment<'a> {
    target: ast::RcLocal<'a>,
    value: ast::RValue<'a>,
}

impl<'a> super::GraphStructurer<'a> {
    // todo: there may be instructions unrelated to the compound conditional earlier in the block
    fn compound_info(&self, node: NodeId) -> Option<CompoundAssignment<'a>> {
        let mut statements = self.function.block(node).unwrap().ast.iter();
        if let Some(assign) = statements.next().unwrap().as_assign() {
            if assign.left.len() != 1 || assign.right.len() != 1 {
                return None;
            }
            let target = assign.left[0].clone();
            if let Ok(target) = target.into_local() {
                if let Some(statement) = statements.next() {
                    if let Some(if_stat) = statement.as_if() {
                        if let ast::RValue::Local(if_condition) = &*if_stat.condition {
                            if &target == if_condition {
                                return Some(CompoundAssignment {
                                    target: target.clone(),
                                    value: assign.right[0].clone().into(),
                                });
                            }
                        }
                    }
                    return None;
                }
                return Some(CompoundAssignment {
                    target,
                    value: assign.right[0].clone().into(),
                });
            }
        }
        return None;
    }

    fn simplify_condition(&mut self, node: NodeId) {
        let block = self.function.block_mut(node).unwrap();
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
        if let Some(unary) = if_stat.condition.as_unary() {
            if_stat.condition = unary.value.clone();
            block.terminator.as_mut().unwrap().swap_edges();
        }
    }

    fn target_expression(
        &mut self,
        first_conditional: NodeId,
        second_conditional: NodeId,
        first_info: Option<CompoundAssignment<'a>>,
        second_info: Option<CompoundAssignment<'a>>,
    ) -> (&mut ast::RValue<'a>, ast::RValue<'a>) {
        let first_block = self.function.block(first_conditional).unwrap();
        let second_block = self.function.block(second_conditional).unwrap();
        if first_block.len() == 2
            && second_block.len() == 2
            && first_info.is_some()
            && second_info.is_some()
            && first_info.as_ref().unwrap().target == second_info.as_ref().unwrap().target
        {
            let assign = ast::Assign::new(
                vec![first_info.as_ref().unwrap().target.clone().into()],
                vec![first_info.as_ref().unwrap().target.clone().into()],
            );
            let block = self.function.block_mut(first_conditional).unwrap();
            block.insert(1, assign.into());
            (
                &mut block
                    .iter_mut()
                    .rev()
                    .skip(1)
                    .next()
                    .unwrap()
                    .as_assign_mut()
                    .unwrap()
                    .right[0],
                second_info.unwrap().value,
            )
        } else {
            let operand = *second_block
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
        first_conditional: NodeId,
        second_conditional: NodeId,
        short_circuit: NodeId,
        end: NodeId,
    ) -> bool {
        self.simplify_condition(first_conditional);
        self.simplify_condition(second_conditional);

        let first_block = self.function.block(first_conditional).unwrap();
        let second_block = self.function.block(second_conditional).unwrap();

        if first_block.len() > 2 || second_block.len() > 2 {
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
        } else {
            if second_else == short_circuit {
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
        }

        self.function
            .replace_edge(&(first_conditional, second_conditional), end);
        self.function.remove_block(second_conditional);

        return true;
    }

    pub fn match_and_or(&mut self, node: NodeId, assigner: NodeId, end: NodeId) -> bool {
        println!("assigner: {} end: {}", assigner, end);
        let info = self.compound_info(assigner);
        if info.is_none() {
            return false;
        }

        self.simplify_condition(node);

        let terminator = self
            .function
            .block(node)
            .unwrap()
            .terminator
            .as_ref()
            .unwrap()
            .as_conditional()
            .unwrap();
        let (then_node, _) = (terminator.0.node, terminator.1.node);

        let info = info.unwrap();

        let binary = ast::Binary::new(
            info.target.clone().into(),
            info.value.clone(),
            if then_node == assigner {
                ast::BinaryOperation::And
            } else {
                ast::BinaryOperation::Or
            },
        );
        let assign = ast::Assign::new(vec![info.target.clone().into()], vec![binary.into()]);
        let block = self.function.block_mut(node).unwrap();
        block.pop();
        block.push(assign.into());

        self.function
            .set_block_terminator(node, Some(Terminator::jump(end)));
        self.function.remove_block(assigner);

        return true;
    }

    pub(crate) fn match_compound_conditional(
        &mut self,
        entry: NodeId,
        then_node: NodeId,
        else_node: NodeId,
    ) -> bool {
        assert!(!self.is_loop_header(then_node));
        assert!(!self.is_loop_header(else_node));

        let graph = self.function.graph();
        let else_successors = graph.successors(else_node);
        let then_successors = graph.successors(then_node);

        let mut changed = false;
        if else_successors.contains(&then_node) && graph.predecessors(else_node).len() == 1 {
            if else_successors.len() == 2 {
                let end = *else_successors.iter().find(|&&n| n != then_node).unwrap();
                changed = self.combine_conditionals(entry, else_node, then_node, end);
            } else {
                changed = self.match_and_or(entry, else_node, then_node);
            }
        } else if then_successors.contains(&else_node) && graph.predecessors(then_node).len() == 1 {
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
