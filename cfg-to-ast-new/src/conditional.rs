use graph::NodeId;

impl<'a> super::GraphStructurer<'a> {
    pub(crate) fn match_conditional(
        &mut self,
        entry: NodeId,
        then_node: NodeId,
        else_node: NodeId,
    ) {
        let mut then_block = None;
        let mut else_block = None;
        let mut exit_block = None;

        // check for continue/break
        let mut then_virtual = self.match_virtual_branch(entry, then_node);
        let mut else_virtual = self.match_virtual_branch(entry, else_node);
        println!("{} {}", then_virtual.is_some(), else_virtual.is_some());

        let then_successors = self.graph.successors(then_node).collect::<Vec<_>>();
        let then_predecessors = self.graph.predecessors(then_node).collect::<Vec<_>>();
        let else_successors = self.graph.successors(else_node).collect::<Vec<_>>();
        let else_predecessors = self.graph.predecessors(else_node).collect::<Vec<_>>();

        if then_virtual.is_none() && else_virtual.is_none() {
            // conditional?
            // todo: check dfs tree instead?
            if ((then_successors.len() == 1
                && else_successors.len() == 1
                && then_successors[0] == else_successors[0])
                || then_successors.is_empty() && else_successors.is_empty())
                && then_predecessors.len() == 1
                && else_predecessors.len() == 1
            {
                // if then else end
                then_block = Some(self.blocks.remove(&then_node).unwrap());
                else_block = Some(self.blocks.remove(&else_node).unwrap());
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
                then_block = Some(self.blocks.remove(&then_node).unwrap());
                exit_block = Some(self.blocks.remove(&else_node).unwrap());
                self.graph.remove_node(then_node).unwrap();
                self.graph.remove_node(else_node).unwrap();
            } else if else_successors.len() == 1
                && then_node == else_successors[0]
                && self.graph.predecessors(then_node).count() == 2
                && self.graph.predecessors(else_node).count() == 1
            {
                else_block = Some(self.blocks.remove(&else_node).unwrap());
                exit_block = Some(self.blocks.remove(&then_node).unwrap());
                self.graph.remove_node(else_node).unwrap();
                self.graph.remove_node(then_node).unwrap();
            } else {
                panic!("no pattern matched");
            }
        } else if then_virtual.is_some() != else_virtual.is_some() {
            // one branch has a virtual edge
            if else_virtual.is_some() {
                std::mem::swap(&mut then_virtual, &mut else_virtual);
            }
            println!("{}", else_node);
            exit_block = Some(self.blocks.remove(&else_node).unwrap());
            else_block = None;
        } else {
            panic!("no pattern matched");
        }

        let block = self.blocks.get_mut(&entry).unwrap();
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = then_block;
        if_stat.else_block = else_block;

        /*if then_block.is_none() && else_block.is_some() {
            std::mem::swap(&mut then_block, &mut else_block);
            if_stat.condition = Box::new(
                ast::Unary::new(*if_stat.condition.clone(), ast::UnaryOperation::Not).into(),
            );
        }


        if let Some(exit_statement) =
            optimizer::virtual_edge_elision::optimize_if_statement(if_stat)
        {
            assert!(exit_block.is_none());
            block.push(exit_statement);
        } else if let Some(exit_block) = exit_block {
            block.extend(exit_block.0);
        }*/
    }
}
