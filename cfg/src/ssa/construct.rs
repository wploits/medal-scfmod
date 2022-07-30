use ast::LocalRw;
use fxhash::FxHashMap;
use graph::{
    algorithms::{
        dfs_tree,
        dominators::{compute_dominance_frontiers, compute_immediate_dominators},
    },
    NodeId,
};

use crate::{
    block::{BasicBlockEdge, Terminator},
    function::Function,
    ssa_def_use,
};

struct SsaConstructor<'a> {
    function: &'a mut Function,
    locals: FxHashMap<ast::RcLocal, Vec<ast::RcLocal>>,
    idoms: FxHashMap<NodeId, NodeId>,
    assignments: FxHashMap<NodeId, Vec<(ast::RcLocal, usize)>>,
}

impl<'a> SsaConstructor<'a> {
    fn edges(&self, node: NodeId) -> Vec<&BasicBlockEdge> {
        self.function
            .blocks()
            .iter()
            .flat_map(|(_, block)| match &block.terminator {
                Some(Terminator::Jump(edge)) => vec![edge],
                Some(Terminator::Conditional(then_edge, else_edge)) => vec![then_edge, else_edge],
                _ => vec![],
            })
            .filter(|edge| edge.node == node)
            .collect::<Vec<_>>()
    }

    fn ssa_defined_in_block(
        &self,
        node: NodeId,
        skip: usize,
        old_local: &ast::RcLocal,
    ) -> Option<ast::RcLocal> {
        if let Some(new_locals) = self.locals.get(&old_local) {
            let block = self.function.block(node).unwrap();
            for statement in block.iter().rev().skip(skip) {
                for written in statement.values_written() {
                    if new_locals.contains(written) {
                        return Some(written.clone());
                    }
                }
            }
        }
        None
    }

    fn find_ssa_variable(
        &self,
        node: NodeId,
        skip: usize,
        old_local: ast::RcLocal,
    ) -> Option<ast::RcLocal> {
        let block_defined = self.ssa_defined_in_block(node, skip, &old_local);
        if block_defined.is_some() {
            return block_defined;
        }
        let new_locals = &self.locals[&old_local];
        for edge in self.edges(node) {
            for argument in &edge.arguments {
                if new_locals.contains(&argument.0) {
                    return Some(argument.0.clone());
                }
            }
        }
        if let Some(&idom) = self.idoms.get(&node) {
            return self.find_ssa_variable(idom, 0, old_local);
        }
        None
    }

    fn replace_reads(&mut self) {
        for node in self.function.graph().nodes().clone() {
            let block_len = self.function.block_mut(node).unwrap().len();
            for index in 0..block_len {
                let mut statement = self.function.block_mut(node).unwrap().remove(index);
                for read in statement.values_read_mut().into_iter() {
                    let ssa_variable =
                        self.find_ssa_variable(node, block_len - index - 1, read.clone());
                    *read = ssa_variable.unwrap()
                }
                self.function
                    .block_mut(node)
                    .unwrap()
                    .insert(index, statement);
            }
        }
    }

    fn remove_unused_parameters(&mut self) {
        let def_use = ssa_def_use::SsaDefUse::new(self.function);

        let to_remove = def_use
            .parameters
            .into_iter()
            .filter(|(local, _)| !def_use.references.contains_key(local))
            .collect::<Vec<_>>();

        for (local, locations) in to_remove {
            for edge in locations {
                match self.function.block_mut(edge.0).unwrap().terminator.as_mut() {
                    Some(Terminator::Jump(edge)) => {
                        edge.arguments.retain(|target, _| target.0 != local);
                    }
                    Some(Terminator::Conditional(then_edge, else_edge)) => {
                        if then_edge.node == edge.1 {
                            then_edge.arguments.retain(|target, _| target.0 != local);
                        } else if else_edge.node == edge.1 {
                            else_edge.arguments.retain(|target, _| target.0 != local);
                        } else {
                            unreachable!();
                        }
                    }
                    None => {}
                }
            }
        }
    }

    fn replace_edge_arguments(&self, node: NodeId, edge: &mut BasicBlockEdge) {
        for argument in edge.arguments.iter_mut() {
            if let Some(ssa_variable) = self.find_ssa_variable(node, 0, argument.1.clone()) {
                *argument.1 = ssa_variable;
            }
        }
    }

    fn replace_block_arguments(&mut self) {
        for node in self.function.graph().nodes().clone() {
            let mut terminator =
                std::mem::take(&mut self.function.block_mut(node).unwrap().terminator);
            match terminator.as_mut() {
                Some(Terminator::Jump(edge)) => {
                    self.replace_edge_arguments(node, edge);
                }
                Some(Terminator::Conditional(then_edge, else_edge)) => {
                    self.replace_edge_arguments(node, then_edge);
                    self.replace_edge_arguments(node, else_edge);
                }
                None => {}
            }
            self.function.block_mut(node).unwrap().terminator = terminator;
        }
    }

    fn create_block_arguments(&mut self) {
        let graph = self.function.graph();
        let entry = self.function.entry().unwrap();
        let dfs = dfs_tree(graph, entry);
        let idoms = compute_immediate_dominators(graph, entry, &dfs);
        let dominance_frontiers = compute_dominance_frontiers(graph, entry, &idoms, &dfs).unwrap();

        for (&node, writes) in &self.assignments {
            for write in writes {
                let new_name = self.function.local_allocator.allocate();
                self.locals
                    .get_mut(&write.0)
                    .unwrap()
                    .push(new_name.clone());
                if let Some(df) = dominance_frontiers.get(&node) {
                    for &df_node in df {
                        for df_pred in self.function.graph().predecessors(df_node) {
                            match self
                                .function
                                .block_mut(df_pred)
                                .unwrap()
                                .terminator
                                .as_mut()
                                .unwrap()
                            {
                                Terminator::Jump(edge) => {
                                    assert!(edge.node == df_node);
                                    edge.arguments.insert(new_name.clone(), write.0.clone());
                                }
                                Terminator::Conditional(then_edge, else_edge) => {
                                    if then_edge.node == df_node {
                                        then_edge
                                            .arguments
                                            .insert(new_name.clone(), write.0.clone());
                                    } else if else_edge.node == df_node {
                                        else_edge
                                            .arguments
                                            .insert(new_name.clone(), write.0.clone());
                                    } else {
                                        unreachable!();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn replace_writes(&mut self) {
        for (&node, writes) in &self.assignments {
            for write in writes {
                let new_local = self.function.local_allocator.allocate();
                self.locals
                    .entry(write.0.clone())
                    .or_insert_with(Vec::new)
                    .push(new_local.clone());
                self.function
                    .block_mut(node)
                    .unwrap()
                    .get_mut(write.1)
                    .unwrap()
                    .as_assign_mut()
                    .unwrap()
                    .replace_values_written(&write.0, &new_local);
            }
        }
    }

    fn construct(mut self) {
        for (&node, block) in self.function.blocks_mut() {
            for (index, statement) in block.iter().enumerate() {
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for local in written {
                    self.assignments
                        .entry(node)
                        .or_insert_with(Vec::new)
                        .push((local, index));
                }
            }
        }

        self.replace_writes();
        self.create_block_arguments();
        self.replace_reads();
        crate::dot::render_to(self.function, &mut std::io::stdout());
        self.replace_block_arguments();
        self.remove_unused_parameters();
    }
}

pub fn construct<'a>(function: &mut Function) {
    let idoms = compute_immediate_dominators(
        function.graph(),
        function.entry().unwrap(),
        &dfs_tree(function.graph(), function.entry().unwrap()),
    );
    SsaConstructor {
        function,
        locals: FxHashMap::default(),
        idoms,
        assignments: FxHashMap::default(),
    }
    .construct();
}
