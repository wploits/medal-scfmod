use std::{iter, borrow::Cow};

use ast::{replace_locals::replace_locals, LocalRw, RcLocal, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::{IndexMap, IndexSet};
use petgraph::{
    stable_graph::NodeIndex,
    visit::{Dfs, Walker},
    Direction,
};

use crate::{
    block::Terminator, function::Function, ssa::param_dependency_graph::ParamDependencyGraph,
};

use super::upvalues::UpvaluesOpen;

struct SsaConstructor<'a> {
    function: &'a mut Function,
    dfs: IndexSet<NodeIndex>,
    incomplete_params: FxHashMap<NodeIndex, FxHashMap<RcLocal, RcLocal>>,
    filled_blocks: FxHashSet<NodeIndex>,
    sealed_blocks: FxHashSet<NodeIndex>,
    current_definition: FxHashMap<RcLocal, FxHashMap<NodeIndex, RcLocal>>,
    all_definitions: FxHashMap<RcLocal, FxHashSet<RcLocal>>,
    old_locals: FxHashMap<RcLocal, RcLocal>,
    local_count: usize,
    local_map: FxHashMap<RcLocal, RcLocal>,
    new_upvalues_in: IndexMap<RcLocal, FxHashSet<RcLocal>>,
    upvalues_passed: FxHashMap<RcLocal, FxHashMap<(NodeIndex, usize), FxHashSet<RcLocal>>>,
}

// https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/phi_cleaner.py
pub fn remove_unnecessary_params(
    function: &mut Function,
    local_map: &mut FxHashMap<RcLocal, RcLocal>,
) {
    for node in function.blocks().map(|(i, _)| i).collect::<Vec<_>>() {
        let mut dependency_graph = ParamDependencyGraph::new(function, node);
        let mut removable_params = FxHashMap::default();
        let mut edges = function.edges_to_block_mut(node);
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
                        if let Some(&param_node) = dependency_graph.local_to_node.get(param) {
                            dependency_graph.remove_node(param_node);
                        }
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

pub fn apply_local_map(function: &mut Function, local_map: FxHashMap<RcLocal, RcLocal>) {
    // TODO: blocks_mut
    for node in function.graph().node_indices().collect::<Vec<_>>() {
        let block = function.block_mut(node).unwrap();
        for stat in block.ast.iter_mut() {
            // TODO: figure out values_mut
            for (from, mut to) in stat
                .values_written_mut()
                .into_iter()
                .filter_map(|v| local_map.get(v).map(|t| (v, t)))
            {
                while let Some(to_to) = local_map.get(to) {
                    to = to_to;
                }
                *from = to.clone();
            }
            let mut map = FxHashMap::default();
            for (from, mut to) in stat
                .values_read_mut()
                .into_iter()
                .filter_map(|v| local_map.get(v).map(|t| (v, t)))
            {
                while let Some(to_to) = local_map.get(to) {
                    to = to_to;
                }
                map.insert(from.clone(), to.clone());
                *from = to.clone();
            }
            stat.traverse_rvalues(&mut |rvalue| {
                if let Some(closure) = rvalue.as_closure_mut() {
                    replace_locals(&mut closure.body, &map)
                }
            });
        }
        if let Some(terminator) = block.terminator_mut() {
            for edge in terminator.edges_mut() {
                // TODO: rename Stat::values, Expr::values to locals() and refer to locals as locals everywhere
                for local in edge
                    .arguments
                    .iter_mut()
                    .flat_map(|(p, a)| iter::once(p).chain(iter::once(a)))
                {
                    if let Some(mut new_local) = local_map.get(local) {
                        while let Some(new_to) = local_map.get(new_local) {
                            new_local = new_to;
                        }
                        *local = new_local.clone();
                    }
                }
            }
        }
    }
}

// based on "Simple and Efficient Construction of Static Single Assignment Form" (https://pp.info.uni-karlsruhe.de/uploads/publikationen/braun13cc.pdf)
impl<'a> SsaConstructor<'a> {
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

    fn add_param_args(
        &mut self,
        node: NodeIndex,
        local: &RcLocal,
        param_local: RcLocal,
    ) -> RcLocal {
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
                    edge.arguments.push((param_local.clone(), argument_local));
                }
                Terminator::Conditional(then_edge, else_edge) => {
                    if then_edge.node == node {
                        then_edge
                            .arguments
                            .push((param_local.clone(), argument_local));
                    } else if else_edge.node == node {
                        else_edge
                            .arguments
                            .push((param_local.clone(), argument_local));
                    } else {
                        unreachable!();
                    }
                }
            }
        }

        self.try_remove_trivial_param(node, param_local)
    }

    fn try_remove_trivial_param(&mut self, node: NodeIndex, param_local: RcLocal) -> RcLocal {
        let mut same = None;
        let edges = self.function.edges_to_block(node);
        let args_in = edges.into_iter().map(|(_, e)| {
            &e.arguments
                .iter()
                .find(|(p, _)| p == &param_local)
                .unwrap()
                .1
        });
        for mut arg in args_in {
            while let Some(arg_to) = self.local_map.get(arg) {
                arg = arg_to;
            }

            if Some(&arg) == same.as_ref() || arg == &param_local {
                // unique value or self-reference
                continue;
            }
            if same.is_some() {
                // the param merges at least two values: not trivial
                return param_local;
            }
            same = Some(arg);
        }
        let same = same.unwrap().clone();
        self.local_map.insert(param_local.clone(), same.clone());

        // TODO: optimize
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            let mut edges = self
                .function
                .edges_to_block(node)
                .into_iter()
                .map(|(_, e)| e)
                .peekable();
            if edges
                .peek()
                .map(|e| !e.arguments.is_empty())
                .unwrap_or(false)
                && edges.any(|e| e.arguments.iter().any(|(_, a)| a == &param_local))
            {
                let edges = self
                    .function
                    .edges_to_block(node)
                    .into_iter()
                    .map(|(_, e)| e)
                    .cloned()
                    .collect::<Vec<_>>();
                let params = edges[0]
                    .arguments
                    .iter()
                    .map(|(p, _)| p)
                    .collect::<Vec<_>>();
                for mut param in params {
                    while let Some(param_to) = self.local_map.get(param) {
                        param = param_to;
                    }

                    if param == &param_local
                        || edges
                            .iter()
                            .any(|e| e.arguments.iter().any(|(p, _)| p == param))
                    {
                        self.try_remove_trivial_param(node, param.clone());
                    }
                }
            }
        }

        same
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
                // TODO: this code is repeated multiple times, create new_local function
                let param_local = self.function.local_allocator.borrow_mut().allocate();
                self.old_locals.insert(param_local.clone(), local.clone());
                if let Some(upvalues) = self.new_upvalues_in.get_mut(local) {
                    upvalues.insert(param_local.clone());
                }
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
                    if let Some(upvalues) = self.new_upvalues_in.get_mut(local) {
                        upvalues.insert(param_local.clone());
                    }
                    self.local_count += 1;
                    self.write_local(node, local, &param_local);

                    self.add_param_args(node, local, param_local)
                }
            }
        };
        self.write_local(node, local, &res);
        res
    }

    // block-local copy propagation
    // TODO: do in local inlining instead since blocks are merged
    // we can also propagate copies where we can guarantee there is no overlap of locals that correspond to the same
    // original local
    // for example:
    /*
    local a = a
    local b
    if g then b = "hi" else b = a end
    return b
    */
    fn propagate_copies(&mut self) {
        // TODO: repeated in inline.rs, move to separate function
        let node_indices = self.function.graph().node_indices().collect::<Vec<_>>();
        let mut local_usages = FxHashMap::default();
        for &node in &node_indices {
            let block = self.function.block(node).unwrap();
            for stat in &block.ast.0 {
                for read in stat.values_read() {
                    local_usages
                        .entry(read.clone())
                        .or_insert_with(FxHashSet::default)
                        .insert(node);
                }
            }
            if let Some(terminator) = block.terminator() {
                for edge in terminator.edges() {
                    for (_, arg) in &edge.arguments {
                        local_usages
                            .entry(arg.clone())
                            .or_insert_with(FxHashSet::default)
                            .insert(node);
                    }
                }
            }
        }
        for node in node_indices {
            let block = self.function.block_mut(node).unwrap();
            let mut assigned_locals = FxHashSet::default();
            let mut indices_to_remove = Vec::new();
            for index in block
                .ast
                .iter()
                .enumerate()
                .filter_map(|(i, s)| s.as_assign().map(|_| i))
                .collect::<Vec<_>>()
            {
                let block = self.function.block_mut(node).unwrap();
                let assign = block.ast[index].as_assign().unwrap();
                if assign.left.len() == 1 && assign.right.len() == 1
                    && let Some(from) = assign.left[0].as_local()
                    && let from_old = &self.old_locals[from]
                    && !self.new_upvalues_in.contains_key(from_old)
                    && !self.upvalues_passed.contains_key(from_old)
                    && let Some(mut to) = assign.right[0].as_local()
                {
                    // TODO: wtf is this name lol
                    while let Some(to_to) = self.local_map.get(to) {
                        to = to_to;
                    }
                    if local_usages[from].len() == 1 || assigned_locals.contains(to) {
                        self.local_map.insert(from.clone(), to.clone());
                        indices_to_remove.push(index);
                    }
                }
                for lvalue in &assign.left {
                    if let Some(assigned) = lvalue.as_local() {
                        assigned_locals.insert(assigned.clone());
                    }
                }
            }
            let block = self.function.block_mut(node).unwrap();
            for index in indices_to_remove.into_iter().rev() {
                block.ast.remove(index);
            }
        }
    }

    fn mark_upvalues(&mut self) {
        let upvalues_open = UpvaluesOpen::new(self.function, self.old_locals.clone());
        for &node in &self.dfs {
            for stat_index in 0..self.function.block(node).unwrap().ast.len() {
                let statement = self
                    .function
                    .block(node)
                    .unwrap()
                    .ast
                    .get(stat_index)
                    .unwrap();
                let values = statement.values().into_iter().cloned().collect::<Vec<_>>();
                for value in values {
                    let old_local = &self.old_locals[&value];
                    if let Some(open_locations) = upvalues_open
                        .open
                        .get(&node)
                        .and_then(|m| m.get(old_local))
                        .and_then(|m| m.get(&stat_index))
                    {
                        if let Some(new_upvalues_in) = self.new_upvalues_in.get_mut(old_local) {
                            assert!(new_upvalues_in.contains(&value));
                        } else {
                            self.upvalues_passed
                                .entry(old_local.clone())
                                .or_default()
                                .entry(*open_locations.first().unwrap())
                                .or_default()
                                .insert(value);
                        }
                    }
                }
            }
            self.function
                .block_mut(node)
                .unwrap()
                .ast
                .retain(|statement| !matches!(statement, ast::Statement::Close(_)))
        }
    }

    fn construct(mut self) -> (usize, Vec<FxHashSet<RcLocal>>, Vec<FxHashSet<RcLocal>>) {
        let entry = self.function.entry().unwrap();
        let mut visited_nodes = Vec::with_capacity(self.function.graph().node_count());
        for i in 0..self.dfs.len() {
            let node = self.dfs[i];
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
                let written = statement
                    .values_written()
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                let mut map = FxHashMap::default();

                for local in &read {
                    let new_local = self.find_local(node, local);
                    map.insert(local.clone(), new_local);
                }
                for (local_index, local) in read.into_iter().enumerate() {
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .ast
                        .get_mut(stat_index)
                        .unwrap();
                    *statement.values_read_mut()[local_index] = map[&local].clone();
                }

                let statement = self
                    .function
                    .block_mut(node)
                    .unwrap()
                    .ast
                    .get_mut(stat_index)
                    .unwrap();
                if let Some(assign) = statement.as_assign()
                    && assign.left.len() == 1
                    && assign.right.len() == 1
                    && let Some(local) = assign.left[0].as_local().cloned()
                    && assign.right[0].as_closure().is_some()
                {
                    let new_local = self.function.local_allocator.borrow_mut().allocate();
                    self.old_locals.insert(new_local.clone(), local.clone());
                    if let Some(upvalues) = self.new_upvalues_in.get_mut(&local) {
                        upvalues.insert(new_local.clone());
                    }
                    self.local_count += 1;
                    self.write_local(node, &local, &new_local);
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .ast
                        .get_mut(stat_index)
                        .unwrap();
                    let assign = statement.as_assign_mut().unwrap();
                    *assign.left[0].as_local_mut().unwrap() = new_local.clone();
                    if let Some(upvalue) = assign.right[0]
                        .as_closure_mut()
                        .unwrap()
                        .upvalues
                        .iter_mut()
                        .find(|u| self.old_locals[u] == local)
                    {
                        *upvalue = new_local.clone();
                        map.insert(local, new_local);
                    }
                } else {
                    // write
                    for (local_index, local) in written.iter().enumerate() {
                        let new_local = self.function.local_allocator.borrow_mut().allocate();
                        self.old_locals.insert(new_local.clone(), local.clone());
                        if let Some(upvalues) = self.new_upvalues_in.get_mut(local) {
                            upvalues.insert(new_local.clone());
                        }
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

                if !map.is_empty() {
                    let statement = self
                        .function
                        .block_mut(node)
                        .unwrap()
                        .ast
                        .get_mut(stat_index)
                        .unwrap();
                    statement.traverse_rvalues(&mut |rvalue| {
                        if let Some(closure) = rvalue.as_closure_mut() {
                            replace_locals(&mut closure.body, &map)
                        }
                    });
                }
            }
            self.filled_blocks.insert(node);

            for &node in &visited_nodes {
                if !self.sealed_blocks.contains(&node)
                    && !self
                        .function
                        .predecessor_blocks(node)
                        .any(|p| !self.filled_blocks.contains(&p))
                    && self.function.predecessor_blocks(node).next().is_some()
                {
                    if let Some(incomplete_params) = self.incomplete_params.remove(&node) {
                        for (local, param_local) in incomplete_params {
                            if !self.new_upvalues_in.contains_key(&local) {
                                self.add_param_args(node, &local, param_local);
                            }
                        }
                    }
                    self.sealed_blocks.insert(node);
                }
            }
        }

        if let Some(mut incomplete_params) = self.incomplete_params.remove(&entry) {
            for param in &mut self.function.parameters {
                *param = incomplete_params
                    .remove(param)
                    .unwrap_or_else(|| self.function.local_allocator.borrow_mut().allocate());
            }
        }
        //println!("{:#?}", self.incomplete_params);
        //crate::dot::render_to(self.function, &mut std::io::stdout()).unwrap();
        assert!(self.incomplete_params.is_empty());

        // TODO: irreducible control flow (see the paper this algorithm is from)
        // TODO: apply_local_map unnecessary number of calls
        apply_local_map(self.function, std::mem::take(&mut self.local_map));
        self.mark_upvalues();
        self.propagate_copies();
        apply_local_map(self.function, std::mem::take(&mut self.local_map));

        //crate::dot::render_to(self.function, &mut std::io::stdout()).unwrap();

        remove_unnecessary_params(self.function, &mut self.local_map);
        apply_local_map(self.function, std::mem::take(&mut self.local_map));
        //println!("{:#?}", self.old_locals);

        (
            self.local_count,
            self.all_definitions.into_values().collect(),
            self.new_upvalues_in
                .into_values()
                .chain(
                    self.upvalues_passed
                        .into_values()
                        .flat_map(|m| m.into_values()),
                )
                .collect(),
        )
    }
}

pub fn construct(
    function: &mut Function,
    upvalues_in: &Vec<RcLocal>,
) -> (usize, Vec<FxHashSet<RcLocal>>, Vec<FxHashSet<RcLocal>>) {
    let mut new_upvalues_in = IndexMap::with_capacity(upvalues_in.len());
    for upvalue in upvalues_in {
        new_upvalues_in.insert(upvalue.clone(), FxHashSet::default());
    }

    let dfs = Dfs::new(function.graph(), function.entry().unwrap())
        .iter(function.graph())
        .collect::<IndexSet<_>>();

    // remove all nodes that will never execute
    for node in function.blocks().map(|(n, _)| n).collect::<Vec<_>>() {
        if !dfs.contains(&node) {
            function.remove_block(node);
        }
    }

    let node_count = function.graph().node_count();
    SsaConstructor {
        function,
        dfs,
        incomplete_params: FxHashMap::with_capacity_and_hasher(node_count, Default::default()),
        filled_blocks: FxHashSet::with_capacity_and_hasher(node_count, Default::default()),
        sealed_blocks: FxHashSet::with_capacity_and_hasher(node_count, Default::default()),
        current_definition: FxHashMap::default(),
        all_definitions: FxHashMap::default(),
        old_locals: FxHashMap::default(),
        local_count: 0,
        local_map: FxHashMap::default(),
        new_upvalues_in,
        upvalues_passed: FxHashMap::default(),
    }
    .construct()
}
