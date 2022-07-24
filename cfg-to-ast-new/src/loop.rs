use graph::{
    algorithms::{dfs_tree, dominators::post_dominator_tree},
    NodeId,
};

use crate::GraphStructurer;

impl<'a> GraphStructurer<'a> {
    fn is_loop_header(&self, node: NodeId) -> bool {
        self.back_edges.iter().any(|edge| edge.1 == node)
    }

    fn refine_breaks(&mut self, header: NodeId, exit: NodeId, exit_predecessors: Vec<NodeId>) {
        for predecessor in exit_predecessors {
            if predecessor != header {
                self.function.remove_edge(&(predecessor, exit));
            }
        }
    }

    fn try_match_natural_loop(&mut self, header: NodeId) -> bool {
        let successors = self.function.graph().successors(header);
        if successors.len() == 1 && successors[0] == header {
            let blocks = self.function.blocks_mut();
            let while_stat = ast::While::new(
                ast::Literal::Boolean(true).into(),
                blocks[&header].ast.clone(),
            );
            blocks.get_mut(&header).unwrap().ast = ast::Block::from_vec(vec![while_stat.into()]);
            self.function.remove_edge(&(header, header));
            true
        } else {
            false
        }
    }

    pub(crate) fn try_collapse_loop(&mut self, header: NodeId) -> bool {
        if !self.is_loop_header(header) {
            return false;
        }

        let graph = self.function.graph();
        let post_dom_tree = post_dominator_tree(graph, &dfs_tree(graph, self.root));

        if self.try_match_natural_loop(header) {
            return true;
        }

        let exit_node = post_dom_tree.predecessors(header).first().cloned();

        /*let exit_statements = post_dom_tree
        .predecessors(header)
        .first()
        .map(|&loop_exit| {
            let exit_predecessors = self.function.graph().predecessors(loop_exit);
            if exit_predecessors.len() > 1 {
                //self.refine_breaks(header, loop_exit, exit_predecessors);
            }
            self.function.remove_block(loop_exit).unwrap().ast
        });*/

        let graph = self.function.graph();

        /*let mut block = self.blocks.remove(&header).unwrap();
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
        if_stat.then_block = Some(ast::Block::from_vec(vec![ast::Break {}.into()]));
        let while_body = *graph.successors(header).first().unwrap();
        //graph.remove_node(while_body);
        if_stat.else_block = Some(self.blocks.remove(&while_body).unwrap());
        let while_loop = ast::While::new(ast::Literal::Boolean(true).into(), block);
        let mut block = ast::Block::from_vec(vec![while_loop.into()]);

        if let Some(exit) = exit_statements {
            block.extend(exit.0);
        }
        self.blocks.insert(header, block);*/

        // println!("exit: {}", while_loop);

        return true;
    }
}
