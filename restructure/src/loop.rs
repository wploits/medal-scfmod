use std::collections::HashMap;
use itertools::Itertools;
use graph::algorithms::{dfs_tree, dominators::post_dominator_tree};
use graph::NodeId;

use crate::GraphStructurer;
use petgraph::stable_graph::NodeIndex;

impl GraphStructurer {
    pub(crate) fn is_loop_header(&self, node: NodeIndex) -> bool {
        self.back_edges.iter().any(|edge| edge.1 == NodeId(node.index()))
    }

    fn refine_breaks(&mut self, header: NodeIndex, exit: NodeIndex, exit_predecessors: Vec<NodeIndex>) {
        for predecessor in exit_predecessors {
            if predecessor != header {
                self.function.remove_edge(predecessor, exit);
            }
        }
    }

    fn try_match_natural_loop(&mut self, header: NodeIndex) -> bool {
        let successors = self.function.successor_blocks(header).collect_vec();
        if successors.len() == 1 && successors[0] == header {
            let mut blocks: HashMap<_, _> = self.function.blocks_mut();
            let while_stat = ast::While::new(
                ast::Literal::Boolean(true).into(),
                blocks[&header].ast.clone(),
            );
            *blocks.get_mut(&header).unwrap().ast = vec![while_stat.into()];
            self.function.remove_edge(header, header);
            true
        } else {
            false
        }
    }

    /*pub(crate) fn try_collapse_loop(&mut self, header: NodeIndex) -> bool {
        return false;

        if !self.is_loop_header(header) {
            return false;
        }

        if self.try_match_natural_loop(header) {
            return true;
        }

        let graph = self.function.graph();
        let post_dom_tree = post_dominator_tree(graph, &dfs_tree(graph, Some(self.root)));
        return false;

        let exit_node = post_dom_tree.predecessors(header).first().cloned();
        if let Some(exit_node) = exit_node {
            let exit_predecessors = graph.predecessors(exit_node);
            if exit_predecessors.len() > 1 {
                self.refine_breaks(header, exit_node, exit_predecessors);
            }
        }

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
    }*/
}
