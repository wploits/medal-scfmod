use ast::{LocalRw, RcLocal};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use petgraph::{matrix_graph::MatrixGraph, stable_graph::NodeIndex};

use crate::function::Function;

use super::liveness::Liveness;

pub struct InterferenceGraph {
    pub graph: MatrixGraph<RcLocal, ()>,
    pub local_to_node: FxHashMap<RcLocal, NodeIndex<u16>>,
}

impl InterferenceGraph {
    pub fn add_node(&mut self, local: RcLocal) -> NodeIndex<u16> {
        let node = self.graph.add_node(local.clone());
        self.local_to_node.insert(local, node);
        node
    }

    fn create_interference_in_nodes(&mut self, variables: &[NodeIndex<u16>]) {
        let len = variables.len();
        for a in 0..len {
            let va = variables[a];
            for &vb in variables.iter().take(len).skip(a + 1) {
                self.graph.update_edge(va, vb, ());
            }
        }
        // for (&a, &b) in variables.iter().tuple_combinations().filter(|(a, b)| a > b) {
        //     self.graph.update_edge(a, b, ());
        // }
    }

    fn create_interference(&mut self, variables: &IndexSet<RcLocal>) {
        let mut nodes = Vec::with_capacity(variables.len());
        for var in variables {
            nodes.push(
                self.local_to_node
                    .get(var)
                    .cloned()
                    .unwrap_or_else(|| self.add_node(var.clone())),
            );
        }
        nodes.sort_unstable();
        self.create_interference_in_nodes(&nodes);
    }

    fn add_edges(&mut self, new_variables: &[RcLocal], current_variables: &IndexSet<RcLocal>) {
        let mut nodes = Vec::with_capacity(new_variables.len());
        for new_var in new_variables {
            let new_var_node = self
                .local_to_node
                .get(new_var)
                .cloned()
                .unwrap_or_else(|| self.add_node(new_var.clone()));
            for current_var in current_variables {
                self.graph
                    .update_edge(self.local_to_node[current_var], new_var_node, ());
            }
            nodes.push(new_var_node);
        }
        nodes.sort_unstable();
        self.create_interference_in_nodes(&nodes);
    }

    fn add_edges_to_nodes(
        &mut self,
        new_variables: &[RcLocal],
        current_variables: &[NodeIndex<u16>],
    ) {
        let mut nodes = Vec::with_capacity(new_variables.len());
        for new_var in new_variables {
            let new_var_node = self
                .local_to_node
                .get(new_var)
                .cloned()
                .unwrap_or_else(|| self.add_node(new_var.clone()));
            for &current_var in current_variables {
                self.graph.update_edge(current_var, new_var_node, ());
            }
            nodes.push(new_var_node);
        }
        nodes.sort_unstable();
        self.create_interference_in_nodes(&nodes);
    }

    fn update_live_set(
        &mut self,
        statement: &ast::Statement,
        current_live_set: &mut IndexSet<RcLocal>,
    ) {
        let mut new_variables = statement
            .values_read()
            .into_iter()
            .cloned()
            .filter(|l| !current_live_set.contains(l))
            .collect::<Vec<_>>();
        // TODO: is collecting to a fxhashset better?
        new_variables.dedup();
        let removed_variables = statement
            .values_written()
            .into_iter()
            .cloned()
            .collect::<IndexSet<_>>();
        let unused_variables = removed_variables
            .difference(current_live_set)
            .cloned()
            .collect::<Vec<_>>();
        if !unused_variables.is_empty() {
            self.add_edges(&unused_variables, current_live_set);
        }
        current_live_set.retain(|l| !removed_variables.contains(l));
        if !new_variables.is_empty() {
            self.add_edges(&new_variables, current_live_set);
        }
        current_live_set.extend(new_variables);
    }

    pub fn new(function: &Function, liveness: &Liveness, local_count: usize) -> Self {
        let mut this = InterferenceGraph {
            graph: MatrixGraph::with_capacity(local_count),
            local_to_node: FxHashMap::with_capacity_and_hasher(local_count, Default::default()),
        };
        for (node, block) in function.blocks() {
            let block_liveness = &liveness.block_liveness[&node];
            let live_in = &block_liveness.live_in;
            let mut live_in_nodes = Vec::with_capacity(live_in.len());
            for var in live_in {
                live_in_nodes.push(
                    this.local_to_node
                        .get(var)
                        .cloned()
                        .unwrap_or_else(|| this.add_node(var.clone())),
                );
            }
            live_in_nodes.sort_unstable();
            this.create_interference_in_nodes(&live_in_nodes);
            this.create_interference(&block_liveness.live_out);
            let mut current_live_set = block_liveness.live_out.clone();
            for statement in block.ast.iter().rev() {
                this.update_live_set(statement, &mut current_live_set);
            }
            let dead_phis = block_liveness
                .defs_phi
                .difference(live_in)
                .cloned()
                .collect::<Vec<_>>();
            this.add_edges_to_nodes(&dead_phis, &live_in_nodes);
        }
        this
    }
}
