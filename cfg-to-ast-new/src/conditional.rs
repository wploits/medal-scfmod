use graph::NodeId;

impl<'a> super::GraphStructurer<'a> {
    pub(crate) fn match_conditional(
        &mut self,
        entry: NodeId,
        then_node: NodeId,
        else_node: NodeId,
    ) {
        let mut if_stat = self
            .blocks
            .get_mut(&entry)
            .unwrap()
            .pop()
            .unwrap()
            .into_if()
            .unwrap();

        let mut exit_block = None;

        // check for continue/break
        let then_virtual = self.match_virtual_branch(entry, then_node);
        let else_virtual = self.match_virtual_branch(entry, else_node);

        let then_successors = self.graph.successors(then_node).collect::<Vec<_>>();
        let then_predecessors = self.graph.predecessors(then_node).collect::<Vec<_>>();
        let else_successors = self.graph.successors(else_node).collect::<Vec<_>>();
        let else_predecessors = self.graph.predecessors(else_node).collect::<Vec<_>>();

        println!("{:?}", then_node);

        if then_virtual.is_none() && else_virtual.is_none() {
            // conditional?
            if ((then_successors.len() == 1
                && else_successors.len() == 1
                && then_successors[0] == else_successors[0])
                || then_successors.is_empty() && else_successors.is_empty())
                && then_predecessors.len() == 1
                && else_predecessors.len() == 1
            {
                if_stat.then_block = Some(self.blocks.remove(&then_node).unwrap());
                if_stat.else_block = Some(self.blocks.remove(&else_node).unwrap());
                self.graph.remove_node(then_node).unwrap();
                self.graph.remove_node(else_node).unwrap();
                if !else_successors.is_empty() {
                    exit_block = Some(self.blocks.remove(&else_successors[0]).unwrap());
                    self.graph.remove_node(else_successors[0]).unwrap();
                }
            }
            // single branch conditional?
            else if then_successors.len() == 1
                && else_node == then_successors[0]
                && self.graph.predecessors(else_node).count() == 2
                && self.graph.predecessors(then_node).count() == 1
            {
                if_stat.then_block = Some(self.blocks.remove(&then_node).unwrap());
                exit_block = Some(self.blocks.remove(&else_node).unwrap());
                self.graph.remove_node(then_node).unwrap();
                self.graph.remove_node(else_node).unwrap();
            } else if else_successors.len() == 1
                && then_node == else_successors[0]
                && self.graph.predecessors(then_node).count() == 2
                && self.graph.predecessors(else_node).count() == 1
            {
                if_stat.else_block = Some(self.blocks.remove(&else_node).unwrap());
                exit_block = Some(self.blocks.remove(&then_node).unwrap());
                self.graph.remove_node(else_node).unwrap();
                self.graph.remove_node(then_node).unwrap();
            } else {
                panic!("no pattern matched");
            }
        } else if then_virtual.is_some() && else_virtual.is_some() {
            if_stat.then_block = then_virtual;
            if_stat.else_block = else_virtual;
        } else if then_virtual.is_some() != else_virtual.is_some() {
            if then_virtual.is_some() {
                if_stat.then_block = then_virtual;
                if_stat.else_block = self.blocks.remove(&else_node);
                self.graph.remove_node(else_node).unwrap();
            } else {
                if_stat.then_block = self.blocks.remove(&then_node);
                if_stat.else_block = else_virtual;
                self.graph.remove_node(then_node).unwrap();
            }
        } else {
            panic!("no pattern matched");
        }

        let block = self.blocks.get_mut(&entry).unwrap();

        if if_stat.then_block.is_some() && if_stat.else_block.is_some() {
            let (last_then_statement, last_else_statement) = (
                if_stat.then_block.as_ref().unwrap().last().unwrap(),
                if_stat.else_block.as_ref().unwrap().last().unwrap(),
            );
            if match last_then_statement {
                ast::Statement::Continue(_) => {
                    matches!(last_else_statement, ast::Statement::Continue(_))
                }
                ast::Statement::Break(_) => matches!(last_else_statement, ast::Statement::Break(_)),
                _ => false,
            } {
                if_stat.then_block.as_mut().unwrap().pop();
                if exit_block.is_none() {
                    exit_block = Some(ast::Block::new());
                }
                exit_block
                    .as_mut()
                    .unwrap()
                    .push(if_stat.else_block.as_mut().unwrap().pop().unwrap());
            }
        }

        // todo: do branches need to be options anymore?
        if if_stat.then_block.is_some() && if_stat.then_block.as_ref().unwrap().is_empty() {
            if_stat.then_block = None;
        }
        if if_stat.else_block.is_some() && if_stat.else_block.as_ref().unwrap().is_empty() {
            if_stat.else_block = None;
        }

        if if_stat.then_block.is_none() && if_stat.else_block.is_some() {
            std::mem::swap(&mut if_stat.then_block, &mut if_stat.else_block);
            if_stat.condition = Box::new(
                ast::Unary::new(*if_stat.condition.clone(), ast::UnaryOperation::Not).into(),
            );
        }

        if if_stat.then_block.is_some() || if_stat.else_block.is_some() {
            block.push(if_stat.into());
        } else {
            // IMPORTANT TODO: evaluate condition if it has side effects
        }
        if let Some(exit_block) = exit_block {
            block.extend(exit_block.0);
        }
    }
}
