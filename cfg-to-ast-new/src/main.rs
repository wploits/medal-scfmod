use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::{dominators::*, *},
    *,
};

struct Block<'a> {
    statements: Vec<ast::Statement<'a>>,
}

struct GraphStructurer<'a> {
    graph: Graph,
    interval_graph: Option<Graph>,
    root: NodeId,
    idoms: &'a FxHashMap<NodeId, NodeId>,
    blocks: FxHashMap<NodeId, ast::Block<'a>>,
}

impl<'a> GraphStructurer<'a> {
    fn new(
        graph: Graph,
        blocks: FxHashMap<NodeId, ast::Block<'a>>,
        root: NodeId,
        idoms: &'a FxHashMap<NodeId, NodeId>,
    ) -> Self {
        Self {
            graph,
            blocks,
            interval_graph: None,
            root,
            idoms,
        }
    }

    fn intervals(&self) -> Graph {
        let mut interval_graph = Graph::default();
        interval_graph.add_node_with_id(self.root).unwrap();
        let back_edges = back_edges(&self.graph, self.root).unwrap();
        for back_edge in &back_edges {
            if back_edge.destination != self.root {
                interval_graph
                    .add_node_with_id(back_edge.destination)
                    .unwrap();
            }
        }
        for back_edge in &back_edges {
            for pred in self.graph.predecessors(back_edge.destination) {
                let mut pred_interval = pred;
                while !interval_graph.node_exists(pred_interval) {
                    pred_interval = self.idoms[&pred_interval];
                }
                if pred_interval != back_edge.destination {
                    interval_graph
                        .add_edge((pred_interval, back_edge.destination).into())
                        .unwrap();
                }
            }
        }
        interval_graph
    }

    //todo: compute node intervals efficiently
    fn node_interval(&self, mut node: NodeId) -> NodeId {
        while !self.interval_graph.as_ref().unwrap().node_exists(node) {
            node = self.idoms[&node];
        }
        node
    }

    fn dfs_of_interval(&self, header: NodeId) -> Graph {
        let mut dfs = dfs_tree(&self.graph, self.root).unwrap();
        for node in dfs.nodes().clone() {
            if self.node_interval(node) != header {
                dfs.remove_node(node).unwrap();
            }
        }
        dfs
    }

    fn collapse_if(
        &mut self,
        dfs: &Graph,
        back_edges: &Vec<Edge>,
        entry: NodeId,
        then_node: NodeId,
        else_node: NodeId,
    ) {
        let (mut then_block, mut else_block, mut exit_block) = (None, None, None);

        let then_successors = self.graph.successors(then_node).collect::<Vec<_>>();
        let else_successors = self.graph.successors(else_node).collect::<Vec<_>>();

        if then_successors.len() == 1
            && else_successors.len() == 1
            && then_successors[0] == else_successors[0]
            && self.graph.predecessors(then_node).count() == 1
            && self.graph.predecessors(else_node).count() == 1
        // && is not cross-interval or back edge
        {
            panic!("diamond pattern");
        } else {
            if then_successors.len() == 1
                && else_node == then_successors[0]
                && self.graph.predecessors(else_node).count() == 2
                && self.graph.predecessors(then_node).count() == 1
            {
                println!("{} consuming {}", entry, then_node);
                println!("{} consuming {}", entry, else_node);
                then_block = Some(self.blocks.remove(&then_node).unwrap());
                exit_block = Some(self.blocks.remove(&else_node).unwrap());
            } else if else_successors.len() == 1
                && then_node == else_successors[0]
                && self.graph.predecessors(then_node).count() == 2
                && self.graph.predecessors(else_node).count() == 1
            {
                println!("{} consuming {}", entry, then_node);
                println!("{} consuming {}", entry, else_node);
                else_block = Some(self.blocks.remove(&else_node).unwrap());
                exit_block = Some(self.blocks.remove(&then_node).unwrap());
            }
            println!(
                "pattern: {} {} {:?} {:?}",
                else_node, then_node, else_successors, then_successors
            );
        }

        let block = self.blocks.get_mut(&entry).unwrap();
        let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();

        if then_block.is_none() && else_block.is_some() {
            std::mem::swap(&mut then_block, &mut else_block);
            if_stat.condition = Box::new(
                ast::Unary::new(*if_stat.condition.clone(), ast::UnaryOperation::Not).into(),
            );
        }

        if let Some(then_block) = then_block {
            if_stat.then_block = Some(then_block);
        }
        if let Some(else_block) = else_block {
            if_stat.else_block = Some(else_block);
        }
        if let Some(exit_block) = exit_block {
            block.extend(exit_block.0);
        }

        /*if !back_edges.contains(&(entry, then_node).into())
            && self.graph.predecessors(then_node).count() == 1
        {
            println!("{} consuming {}", entry, then_node);
            then_block = Some(self.blocks.remove(&then_node).unwrap());
        }

        if !back_edges.contains(&(entry, else_node).into())
            && self.graph.predecessors(else_node).count() == 1
        {
            println!("{} consuming {}", entry, else_node);
            else_block = Some(self.blocks.remove(&else_node).unwrap());
        }*/

        //dot::render_to(&self.graph, &mut std::io::stdout());

        /*if then_block.is_some() || else_block.is_some() {
            if then_successors.len() < 2 && else_successors.len() < 2 {
                println!("yesss");
                if then_successors.len() == 1
                    && else_successors.len() == 1
                    && then_successors[0] == else_successors[0]
                {
                    println!("{} consuming {}", entry, else_node);
                    exit_block = Some(self.blocks.remove(&then_successors[0]).unwrap());
                } else if then_successors[0] == else_node {
                    exit_block = else_block;
                    else_block = None;
                } else if else_successors[0] == then_node {
                    exit_block = then_block;
                    then_block = None;
                } else {
                    panic!("if pattern doesnt match");
                }
            }

            let block = self.blocks.get_mut(&entry).unwrap();
            let if_stat = block.last_mut().unwrap().as_if_mut().unwrap();
            if let Some(then_block) = then_block {
                if_stat.then_block = Some(then_block);
            }
            if let Some(else_block) = else_block {
                if_stat.else_block = Some(else_block);
            }
            if let Some(exit_block) = exit_block {
                block.extend(exit_block.0);
            }
        }*/
    }

    fn collapse_jump(&mut self, back_edges: &[Edge], node: NodeId, target: NodeId) {
        if !back_edges.contains(&(node, target).into())
            && self.graph.predecessors(target).count() == 1
            && self.graph.successors(node).count() == 1
        {
            println!("{} consuming {}", node, target);
            let block = self.blocks.remove(&target).unwrap();
            self.blocks.get_mut(&node).unwrap().extend(block.0);
        }
    }

    fn collapse(&mut self) {
        while self.graph.nodes().len() > 1 {
            let back_edges = back_edges(&self.graph, self.root).unwrap();
            self.interval_graph = Some(self.intervals());
            graph::dot::render_to(
                self.interval_graph.as_ref().unwrap(),
                &mut std::io::stdout(),
            )
            .unwrap();
            let interval_graph_dfs =
                dfs_tree(self.interval_graph.as_ref().unwrap(), self.root).unwrap();
            for header in interval_graph_dfs.post_order(self.root).unwrap() {
                let interval_dfs = self.dfs_of_interval(header);
                for node in interval_dfs.post_order(header).unwrap() {
                    //println!("interval {} node {}", header, node);
                    let successors = self.graph.successors(node).collect::<Vec<_>>();
                    match successors.len() {
                        0 => {}
                        1 => self.collapse_jump(&back_edges, node, successors[0]),
                        2 => self.collapse_if(
                            &interval_dfs,
                            &back_edges,
                            node,
                            successors[1],
                            successors[0],
                        ),
                        _ => panic!(),
                    }
                }
            }
            self.graph = self.interval_graph.take().unwrap();
        }
    }

    fn structure(mut self) -> ast::Block<'a> {
        self.collapse();
        self.blocks.remove(&self.root).unwrap()
    }
}

fn main() {
    let mut blocks = FxHashMap::<NodeId, ast::Block>::default();
    blocks.insert(
        NodeId(1),
        ast::Block::from_vec(vec![
            ast::Assign::new(
                ast::Local::new("x".into()).into(),
                ast::Literal::from("hello world").into(),
            )
            .into(),
            ast::If::new(ast::Local::new("x".into()).into(), None, None).into(),
        ]),
    );
    blocks.insert(
        NodeId(2),
        ast::Block::from_vec(vec![ast::Assign::new(
            ast::Local::new("y".into()).into(),
            ast::Literal::from(1.0).into(),
        )
        .into()]),
    );
    blocks.insert(
        NodeId(3),
        ast::Block::from_vec(vec![ast::Assign::new(
            ast::Local::new("y".into()).into(),
            ast::Literal::from(2.0).into(),
        )
        .into()]),
    );
    blocks.insert(
        NodeId(4),
        ast::Block::from_vec(vec![ast::Assign::new(
            ast::Local::new("z".into()).into(),
            ast::Local::new("y".into()).into(),
        )
        .into()]),
    );
    let graph = Graph::from_edges(vec![(1, 2), (1, 3), (2, 3), (3, 4)]);
    graph::dot::render_to(&graph, &mut std::io::stdout()).unwrap();

    let root = NodeId(1);

    let dfs = dfs_tree(&graph, root).unwrap();
    let idoms = compute_immediate_dominators(&graph, root, &dfs).unwrap();

    let structurer = GraphStructurer::new(graph, blocks, root, &idoms);
    let block = structurer.structure();
    println!("{}", block);
}
