// TODO: have an option to only merge variables with the same register
// if we have that as default, it should increase performance by a bit

use ast::{replace_locals::replace_locals, LocalRw, RcLocal, Traverse};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::{IndexMap, IndexSet};
use petgraph::{
    data::Build,
    prelude::UnGraphMap,
    stable_graph::NodeIndex,
    visit::{Bfs, Walker},
};

use crate::function::Function;

use super::interference_graph::InterferenceGraph;

// https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/variable_renaming.py
// A minimal renaming strategy, that renames the SSA-variables such that the total number of non SSA-variables is (almost) minimal.
// Therefore, we construct color-classes by using lexicographical BFS on the interference graph. When the interference graph is chordal
// this leads to a minimum number of possible variables.
pub struct LocalRenamer<'a> {
    function: &'a mut Function,
    interference_graph: &'a mut InterferenceGraph,
    renaming_map: FxHashMap<RcLocal, RcLocal>,
    new_upvalues: IndexSet<RcLocal>,
    local_nodes: &'a mut FxHashMap<RcLocal, FxHashSet<NodeIndex>>,
}

impl<'a> LocalRenamer<'a> {
    // TODO: STYLE: rename local_groups to local_relations or related_locals
    pub fn new(
        function: &'a mut Function,
        local_groups: &[FxHashSet<RcLocal>],
        upvalue_to_group: &IndexMap<ast::RcLocal, usize>,
        interference_graph: &'a mut InterferenceGraph,
        local_nodes: &'a mut FxHashMap<RcLocal, FxHashSet<NodeIndex>>,
    ) -> Self {
        let mut res = Self {
            function,
            interference_graph,
            renaming_map: FxHashMap::default(),
            local_nodes,
            new_upvalues: IndexSet::default(),
        };
        res.generate_renaming_map(local_groups);
        // TODO: move to separate function
        let mut upvalue_locals = FxHashMap::default();
        for (upvalue, &group) in upvalue_to_group {
            //println!("{:?} {}", upvalue, group);
            let group_local = upvalue_locals
                .entry(group)
                .or_insert_with(|| res.function.local_allocator.borrow_mut().allocate())
                .clone();
            res.renaming_map
                .insert(upvalue.clone(), group_local.clone());
            res.new_upvalues.insert(group_local);
        }

        res
    }

    pub fn rename(mut self) -> IndexSet<RcLocal> {
        let mut dfs = Bfs::new(self.function.graph(), self.function.entry().unwrap());
        while let Some(node) = dfs.next(self.function.graph()) {
            let block = self.function.block_mut(node).unwrap();
            for stat in block.iter_mut() {
                // TODO: values_mut or maybe LocalRw::apply_renaming_map (similar code in construction)
                for (to, from) in stat
                    .values_written_mut()
                    .into_iter()
                    .map(|v| (&self.renaming_map[v], v))
                {
                    *from = to.clone();
                    self.local_nodes.entry(to.clone()).or_default().insert(node);
                }
                stat.traverse_rvalues(&mut |rvalue| {
                    if let Some(closure) = rvalue.as_closure_mut() {
                        replace_locals(&mut closure.body, &self.renaming_map)
                    }
                });
                for (to, from) in stat
                    .values_read_mut()
                    .into_iter()
                    .map(|v| (&self.renaming_map[v], v))
                {
                    *from = to.clone();
                    self.local_nodes.entry(to.clone()).or_default().insert(node);
                }
            }
        }
        self.remove_redundant_assigns();
        for param in &mut self.function.parameters {
            if let Some(new_param) = self.renaming_map.remove(param) {
                *param = new_param;
            }
        }
        self.new_upvalues
    }

    // Remove assignments in the form of "x = x"
    fn remove_redundant_assigns(&mut self) {
        for block in self.function.graph_mut().node_weights_mut() {
            block.retain(|stat| {
                stat.as_assign()
                    .map(|a| {
                        let l = a.left[0].as_local();
                        let r = a.right[0].as_local();
                        !(a.left.len() + a.right.len() == 2 && l == r && l.is_some())
                    })
                    .unwrap_or(true)
            })
        }
    }

    // TODO: REFACTOR: pass local_to_group to this function, we have it in lifter.rs due to inlining
    // requiring it
    fn generate_renaming_map(&mut self, local_groups: &[FxHashSet<RcLocal>]) {
        let local_to_group = local_groups
            .iter()
            .cloned()
            .enumerate()
            .flat_map(|(i, g)| g.into_iter().map(move |l| (l, i)))
            .collect::<FxHashMap<_, _>>();
        let node_count = self.interference_graph.graph.node_count();
        let mut same_group_dependency_graph =
            UnGraphMap::<NodeIndex<u16>, ()>::with_capacity(node_count, 0);
        for node in 0..self.interference_graph.graph.node_count() {
            same_group_dependency_graph.add_node(NodeIndex::new(node));
            let node_group = local_to_group[&self.interference_graph.graph[NodeIndex::new(node)]];
            for neighbor in self
                .interference_graph
                .graph
                .neighbors(NodeIndex::new(node))
            {
                let neighbor_group = local_to_group[&self.interference_graph.graph[neighbor]];
                if neighbor_group == node_group {
                    same_group_dependency_graph.update_edge(
                        NodeIndex::new(node),
                        NodeIndex::new(neighbor.index()),
                        (),
                    );
                }
            }
        }

        let mut visited =
            FxHashSet::<NodeIndex<u16>>::with_capacity_and_hasher(node_count, Default::default());
        for node in same_group_dependency_graph.nodes() {
            if !visited.contains(&node) {
                // TODO: BFS vs DFS speed?
                let component = Bfs::new(&same_group_dependency_graph, node)
                    .iter(&same_group_dependency_graph)
                    .collect::<Vec<_>>();
                visited.extend(&component);

                let component_local = self.function.local_allocator.borrow_mut().allocate();
                for node in component {
                    self.renaming_map.insert(
                        self.interference_graph.graph[node].clone(),
                        component_local.clone(),
                    );
                }
            }
        }
    }
}
