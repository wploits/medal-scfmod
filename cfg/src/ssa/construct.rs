use std::iter;

use ast::{LocalRw, RcLocal};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{stable_graph::NodeIndex, visit::Dfs, Direction};

use crate::{
    block::Terminator, function::Function, ssa::param_dependency_graph::ParamDependencyGraph,
    ssa_def_use,
};

use super::upvalues::UpvaluesOpen;

struct SsaConstructor<'a> {
    function: &'a mut Function,
    incomplete_params: FxHashMap<NodeIndex, FxHashMap<RcLocal, RcLocal>>,
    filled_blocks: FxHashSet<NodeIndex>,
    sealed_blocks: FxHashSet<NodeIndex>,
    current_definition: FxHashMap<RcLocal, FxHashMap<NodeIndex, RcLocal>>,
    all_definitions: FxHashMap<RcLocal, FxHashSet<RcLocal>>,
    old_locals: FxHashMap<RcLocal, RcLocal>,
    local_count: usize,
}

// based on "Simple and Efficient Construction of Static Single Assignment Form" (https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf)
impl<'a> SsaConstructor<'a> {
    fn remove_unused_parameters(&mut self) {
        // TODO: does this ssa algorithm even result in unused parameters?
        let def_use = ssa_def_use::SsaDefUse::new(self.function);

        let to_remove = def_use
            .parameters
            .into_iter()
            .filter(|(local, _)| !def_use.references.contains_key(local));

        for (local, locations) in to_remove {
            for edge in locations {
                match self.function.block_mut(edge.0).unwrap().terminator.as_mut() {
                    Some(Terminator::Jump(edge)) => {
                        edge.arguments.retain(|(target, _)| target != &local);
                    }
                    Some(Terminator::Conditional(then_edge, else_edge)) => {
                        if then_edge.node == edge.1 {
                            then_edge.arguments.retain(|(target, _)| target != &local);
                        } else if else_edge.node == edge.1 {
                            else_edge.arguments.retain(|(target, _)| target != &local);
                        } else {
                            unreachable!();
                        }
                    }
                    None => {}
                }
            }
        }
    }

    fn write_local(&mut self, node: NodeIndex, local: &RcLocal, new_local: &RcLocal) {
        self.all_definitions
            .entry(local.clone())
            .or_default()
            .insert(new_local.clone());
        self.current_definition
            .entry(local.clone())
            .or_default()
            .insert(node, new_local.clone());
    }

    fn add_param_args(&mut self, node: NodeIndex, local: &RcLocal, new_local: &RcLocal) {
        let mut preds = self.function.predecessor_blocks(node).detach();
        while let Some((_, pred)) = preds.next(self.function.graph()) {
            let argument_local = self.find_local(pred, local);
            match self
                .function
                .block_mut(pred)
                .unwrap()
                .terminator
                .as_mut()
                .unwrap()
            {
                Terminator::Jump(edge) => {
                    assert!(edge.node == node);
                    edge.arguments.push((new_local.clone(), argument_local));
                }
                Terminator::Conditional(then_edge, else_edge) => {
                    if then_edge.node == node {
                        then_edge
                            .arguments
                            .push((new_local.clone(), argument_local));
                    } else if else_edge.node == node {
                        else_edge
                            .arguments
                            .push((new_local.clone(), argument_local));
                    } else {
                        unreachable!();
                    }
                }
            }
        }
    }

    fn find_local(&mut self, node: NodeIndex, local: &RcLocal) -> RcLocal {
        let res = if let Some(new_local) = self
            .current_definition
            .get(local)
            .and_then(|x| x.get(&node))
        {
            // local to block
            new_local.clone()
        } else {
            // search globally
            if !self.sealed_blocks.contains(&node) {
                let param_local = self.function.local_allocator.borrow_mut().allocate();
                self.old_locals.insert(param_local.clone(), local.clone());
                self.local_count += 1;
                self.incomplete_params
                    .entry(node)
                    .or_default()
                    .insert(local.clone(), param_local.clone());
                param_local
            } else {
                let mut preds = self.function.predecessor_blocks(node);
                let first_pred = preds.next().unwrap();
                if preds.next().is_none() {
                    self.find_local(first_pred, local)
                } else {
                    let param_local = self.function.local_allocator.borrow_mut().allocate();
                    self.old_locals.insert(param_local.clone(), local.clone());
                    self.local_count += 1;
                    self.write_local(node, local, &param_local);

                    self.add_param_args(node, local, &param_local);
                    // todo: try remove trivial parameter

                    param_local
                }
            }
        };
        self.write_local(node, local, &res);
        res
    }

    fn apply_local_map(&mut self, local_map: &FxHashMap<RcLocal, RcLocal>) {
        // TODO: blocks_mut
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            let block = self.function.block_mut(node).unwrap();
            for stat in block.ast.iter_mut() {
                // TODO: figure out values_mut
                for (from, to) in stat
                    .values_written_mut()
                    .into_iter()
                    .filter_map(|v| local_map.get(v).map(|t| (v, t)))
                {
                    *from = to.clone();
                }
                for (from, to) in stat
                    .values_read_mut()
                    .into_iter()
                    .filter_map(|v| local_map.get(v).map(|t| (v, t)))
                {
                    *from = to.clone();
                }
            }
            if let Some(terminator) = block.terminator_mut() {
                for edge in terminator.edges_mut() {
                    // TODO: rename Stat::values, Expr::values to locals() and refer to locals as locals everywhere
                    for local in edge
                        .arguments
                        .iter_mut()
                        .flat_map(|(p, a)| iter::once(p).chain(iter::once(a)))
                    {
                        if let Some(new_local) = local_map.get(local) {
                            *local = new_local.clone();
                        }
                    }
                }
            }
        }
    }

    fn propagate_copies(&mut self, local_map: &mut FxHashMap<RcLocal, RcLocal>) {
        // TODO: blocks_mut
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            let block = self.function.block_mut(node).unwrap();
            let mut indices_to_remove = Vec::new();
            for (index, assign) in block
                .ast
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.as_assign().map(|a| (i, a)))
            {
                if assign.left.len() == 1 && assign.right.len() == 1
                    && let Some(from) = assign.left[0].0.as_local()
                    && let Some(mut to) = assign.right[0].as_local()
                {
                    // TODO: wtf is this name lol
                    while let Some(to_to) = local_map.get(to) {
                        to = to_to;
                    }
                    local_map.insert(from.clone(), to.clone());
                    indices_to_remove.push(index)
                }
            }
            for index in indices_to_remove.into_iter().rev() {
                block.ast.remove(index);
            }
        }
    }

    // https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/phi_cleaner.py
    fn remove_unnecessary_params(&mut self, local_map: &mut FxHashMap<RcLocal, RcLocal>) {
        for node in self.function.blocks().map(|(i, _)| i).collect::<Vec<_>>() {
            let mut dependency_graph = ParamDependencyGraph::new(self.function, node);
            let mut removable_params = FxHashMap::default();
            let mut edges = self.function.edges_to_block_mut(node);
            if !edges.is_empty() {
                let params = edges[0].arguments.iter().map(|(p, _)| p);
                let args_in_by_block = edges
                    .iter()
                    .map(|e| e.arguments.iter().map(|(_, a)| a).collect::<Vec<_>>())
                    .collect::<Vec<_>>();
                let mut params_to_remove = FxHashSet::default();
                for (index, mut param) in params.enumerate() {
                    let arg_set = args_in_by_block
                        .iter()
                        .map(|a| a[index])
                        .collect::<FxHashSet<_>>();
                    if arg_set.len() == 1 {
                        while let Some(param_to) = local_map.get(param) {
                            param = param_to;
                        }
                        let mut arg = arg_set.into_iter().next().unwrap();
                        while let Some(arg_to) = local_map.get(arg) {
                            arg = arg_to;
                        }
                        if arg != param {
                            // param is not trivial, we must replace the param with the arg
                            // y = phi(x, x, ..., x)
                            removable_params.insert(param.clone(), arg.clone());
                        } else {
                            // param is trivial
                            // x = phi(x, x, ..., x)
                            let param_node = dependency_graph.local_to_node[param];
                            dependency_graph.remove_node(param_node);
                        }

                        params_to_remove.insert(param.clone());
                    }
                }
                for edge in &mut edges {
                    edge.arguments
                        .retain(|(p, _)| !params_to_remove.contains(p))
                }
            }

            let mut removable_params_degree_zero = removable_params
                .iter()
                .map(|(p, a)| (p.clone(), a))
                .filter(|(p, _)| {
                    dependency_graph
                        .graph
                        .neighbors(dependency_graph.local_to_node[p])
                        .count()
                        == 0
                })
                .collect::<Vec<_>>();

            while let Some((param, mut arg)) = removable_params_degree_zero.pop() {
                let param_node = dependency_graph.local_to_node[&param];
                for param_pred_node in dependency_graph
                    .graph
                    .neighbors_directed(param_node, Direction::Incoming)
                {
                    if dependency_graph.graph.neighbors(param_pred_node).count() == 1 {
                        let param_pred = dependency_graph
                            .graph
                            .node_weight(param_pred_node)
                            .unwrap()
                            .clone();
                        if let Some(param_pred_arg) = removable_params.get(&param_pred) {
                            removable_params_degree_zero.push((param_pred, param_pred_arg));
                        }
                    }
                }
                dependency_graph.remove_node(param_node);

                while let Some(arg_to) = local_map.get(arg) {
                    arg = arg_to;
                }
                local_map.insert(param, arg.clone());
            }
        }
    }

    fn fix_upvalues(&mut self, local_map: &mut FxHashMap<ast::RcLocal, ast::RcLocal>) {
        let upvalues_open = UpvaluesOpen::new(self.function, self.old_locals.clone());
        println!("upvalues open: {:#?}", upvalues_open);

        let mut dfs = Dfs::new(self.function.graph(), self.function.entry().unwrap());
        while let Some(node) = dfs.next(self.function.graph()) {
            for stat_index in 0..self.function.block(node).unwrap().ast.len() {
                let statement = self
                    .function
                    .block(node)
                    .unwrap()
                    .ast
                    .get(stat_index)
                    .unwrap();
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for value in written {
                    if let Some(open_local) =
                        upvalues_open.find_open(node, stat_index, &value, self.function)
                    {
                        println!("open local: {}", open_local);
                        local_map.insert(value, open_local);
                    }
                }
            }
            self.function
                .block_mut(node)
                .unwrap()
                .ast
                .retain(|statement| !matches!(statement, ast::Statement::Close(_)))
        }
        let mut changed = true;
        while changed {
            changed = false;
            let locals = local_map
                .iter()
                .filter_map(|(old, new)| {
                    local_map
                        .get(new)
                        .map(|new_local| (old.clone(), new.clone(), new_local.clone()))
                })
                .collect::<Vec<_>>();
            for (old, new, new_local) in locals {
                local_map.insert(old, new_local.clone());
                local_map.insert(new, new_local);
                changed = true;
            }
        }
    }

    fn construct(mut self) -> (usize, Vec<FxHashSet<RcLocal>>) {
        let entry = self.function.entry().unwrap();
        let mut dfs = Dfs::new(self.function.graph(), entry);
        let mut visited_nodes = Vec::with_capacity(self.function.graph().node_count());
        while let Some(node) = dfs.next(self.function.graph()) {
            visited_nodes.push(node);
            for stat_index in 0..self.function.block(node).unwrap().ast.len() {
                // read
                let statement = self
                    .function
                    .block_mut(node)
                    .unwrap()
                    .ast
                    .get_mut(stat_index)
                    .unwrap();
                let read = statement
                    .values_read()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for (local_index, local) in read.into_iter().enumerate() {
                    let new_local = self.find_local(node, &local);
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .ast
                        .get_mut(stat_index)
                        .unwrap();
                    *statement.values_read_mut()[local_index] = new_local;
                }

                // write

                let statement = self
                    .function
                    .block_mut(node)
                    .unwrap()
                    .ast
                    .get_mut(stat_index)
                    .unwrap();
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for (local_index, local) in written.iter().enumerate() {
                    let new_local = self.function.local_allocator.borrow_mut().allocate();
                    self.old_locals.insert(new_local.clone(), local.clone());
                    self.local_count += 1;
                    self.write_local(node, local, &new_local);
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .ast
                        .get_mut(stat_index)
                        .unwrap();
                    *statement.values_written_mut()[local_index] = new_local;
                }
            }
            self.filled_blocks.insert(node);

            for &node in &visited_nodes {
                if !self.sealed_blocks.contains(&node)
                    && !self
                        .function
                        .predecessor_blocks(node)
                        .any(|p| !self.filled_blocks.contains(&p))
                {
                    if let Some(incomplete_params) = self.incomplete_params.remove(&node) {
                        for (local, param_local) in incomplete_params {
                            self.add_param_args(node, &local, &param_local);
                        }
                    }
                    self.sealed_blocks.insert(node);
                }
            }
        }

        self.remove_unused_parameters();
        //crate::dot::render_to(self.function, &mut std::io::stdout()).unwrap();

        // TODO: irreducible control flow (see the paper this algorithm is from)

        let mut local_map = FxHashMap::default();
        //self.propagate_copies(&mut local_map);
        self.remove_unnecessary_params(&mut local_map);
        self.fix_upvalues(&mut local_map);
        self.apply_local_map(&local_map);

        (
            self.local_count,
            self.all_definitions.into_values().collect(),
        )
    }
}

pub fn construct(function: &mut Function) -> (usize, Vec<FxHashSet<RcLocal>>) {
    let node_count = function.graph().node_count();
    SsaConstructor {
        function,
        incomplete_params: FxHashMap::with_capacity_and_hasher(node_count, Default::default()),
        filled_blocks: FxHashSet::with_capacity_and_hasher(node_count, Default::default()),
        sealed_blocks: FxHashSet::with_capacity_and_hasher(node_count, Default::default()),
        current_definition: FxHashMap::default(),
        all_definitions: FxHashMap::default(),
        old_locals: FxHashMap::default(),
        local_count: 0,
    }
    .construct()
}
