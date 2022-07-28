use ast::{LocalRw, RcLocal};
use std::rc::Rc;

use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::{
        dfs_tree,
        dominators::{compute_dominance_frontiers, compute_immediate_dominators, dominator_tree},
    },
    Edge, NodeId,
};

use crate::{
    block::{BasicBlockEdge, Terminator},
    function::Function,
    ssa_def_use,
};

struct SsaConstructor<'a, 'b> {
    function: &'b mut Function<'a>,
    locals: FxHashMap<RcLocal<'a>, Vec<RcLocal<'a>>>,
    idoms: FxHashMap<NodeId, NodeId>,
}

impl<'a, 'b> SsaConstructor<'a, 'b> {
    fn edges(&self, node: NodeId) -> Vec<&BasicBlockEdge<'a>> {
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

    fn find_ssa_variable(
        &mut self,
        node: NodeId,
        index: usize,
        old_local: RcLocal<'a>,
    ) -> RcLocal<'a> {
        let new_locals = self.locals.get(&old_local);
        if new_locals.is_none() {
            return old_local;
        }
        let new_locals = new_locals.unwrap();
        let block = self.function.block(node).unwrap();
        for statement in block.iter().rev().skip(block.len() - index) {
            for write in statement.values_written() {
                if new_locals.contains(write) {
                    return write.clone();
                }
            }
        }
        for edge in self.edges(node) {
            for argument in &edge.arguments {
                if new_locals.contains(&argument.1) {
                    return argument.0.clone();
                }
            }
        }
        if let Some(&idom) = self.idoms.get(&node) {
            return self.find_ssa_variable(
                idom,
                self.function.block(idom).unwrap().len(),
                old_local,
            );
        }
        println!("failed to find value");
        self.function.local_allocator.allocate()
    }

    fn replace_reads(&mut self) {
        for node in self.function.graph().nodes().clone() {
            for index in 0..self.function.block_mut(node).unwrap().len() {
                let mut statement = self.function.block_mut(node).unwrap().remove(index);
                for read in statement
                    .values_read()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>()
                {
                    let ssa_variable = self.find_ssa_variable(
                        node,
                        self.function.block(node).unwrap().len(),
                        read.clone(),
                    );
                    statement.replace_values_read(&read, &ssa_variable);
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
            .map(|(a, b)| (a.0.to_string().clone(), b))
            .collect::<Vec<_>>();

        for (local, locations) in to_remove {
            for edge in locations {
                match self.function.block_mut(edge.0).unwrap().terminator.as_mut() {
                    Some(Terminator::Jump(edge)) => {
                        edge.arguments.retain(|target, _| target.0 != local);
                    }
                    Some(Terminator::Conditional(then_edge, else_edge)) => {
                        if then_edge.node == edge.0 {
                            then_edge.arguments.retain(|target, _| target.0 != local);
                        } else if else_edge.node == edge.0 {
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

    fn construct(mut self) {
        let graph = self.function.graph();
        let entry = self.function.entry().unwrap();

        let dfs = dfs_tree(graph, entry);
        let idoms = compute_immediate_dominators(graph, entry, &dfs);

        let dominance_frontiers = compute_dominance_frontiers(graph, entry, &idoms, &dfs).unwrap();

        let mut assignments = FxHashMap::default();

        for (&node, block) in self.function.blocks_mut() {
            for (index, statement) in block.iter().enumerate() {
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for local in written {
                    assignments
                        .entry(node)
                        .or_insert_with(Vec::new)
                        .push((local, index));
                }
            }
        }

        for (&node, writes) in &assignments {
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

        for (&node, writes) in &assignments {
            for write in writes {
                let new_name = self.function.local_allocator.allocate();
                self.locals
                    .get_mut(&write.0)
                    .unwrap()
                    .push(new_name.clone());
                if let Some(df) = dominance_frontiers.get(&node) {
                    for &df_node in df {
                        for df_pred in self.function.graph().predecessors(df_node) {
                            let argument = self.find_ssa_variable(
                                df_pred,
                                self.function.block(df_pred).unwrap().len(),
                                write.0.clone(),
                            );
                            let terminator = self
                                .function
                                .block_mut(df_pred)
                                .unwrap()
                                .terminator
                                .as_mut()
                                .unwrap();
                            match terminator {
                                Terminator::Jump(edge) => {
                                    assert!(edge.node == df_node);
                                    edge.arguments.insert(new_name.clone(), argument);
                                }
                                Terminator::Conditional(then_edge, else_edge) => {
                                    if then_edge.node == df_node {
                                        then_edge.arguments.insert(new_name.clone(), argument);
                                    } else if else_edge.node == df_node {
                                        else_edge.arguments.insert(new_name.clone(), argument);
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

        crate::dot::render_to(self.function, &mut std::io::stdout());

        self.replace_reads();
        self.remove_unused_parameters();
    }
}

pub fn construct<'a>(function: &mut Function<'a>) {
    crate::dot::render_to(function, &mut std::io::stdout());
    let idoms = compute_immediate_dominators(
        function.graph(),
        function.entry().unwrap(),
        &dfs_tree(function.graph(), function.entry().unwrap()),
    );
    SsaConstructor {
        function,
        locals: FxHashMap::default(),
        idoms,
    }
    .construct();
}
