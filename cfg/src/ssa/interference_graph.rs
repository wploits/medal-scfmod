use ast::LocalRw;
use ast::RcLocal;
use fxhash::FxHashMap;
use graph::{Graph, NodeId, Undirected};
use itertools::Itertools;
use linked_hash_set::LinkedHashSet;

use crate::function::Function;

use super::liveness::Liveness;

pub struct InterferenceGraph {
    pub graph: Graph<Undirected>,
    pub node_to_variable: FxHashMap<NodeId, RcLocal>,
    pub variable_to_node: FxHashMap<RcLocal, NodeId>,
}

impl InterferenceGraph {
    fn create_interference(&mut self, variables: &LinkedHashSet<RcLocal>) {
        for var in variables {
            let new_node = self.graph.add_node();
            self.node_to_variable.insert(new_node, var.clone());
            self.variable_to_node.insert(var.clone(), new_node);
        }
        for combination in variables.iter().combinations(2) {
            self.graph.add_edge((
                self.variable_to_node[combination[0]],
                self.variable_to_node[combination[1]],
            ));
        }
    }

    fn add_edges(&mut self, new_variables: &[RcLocal], current_variables: &LinkedHashSet<RcLocal>) {
        for new_var in new_variables {
            let new_node = self.graph.add_node();
            self.node_to_variable
                .entry(new_node)
                .or_insert_with(|| new_var.clone());
            self.variable_to_node
                .entry(new_var.clone())
                .or_insert_with(|| new_node);
            for current_var in current_variables {
                self.graph.add_edge((
                    self.variable_to_node[current_var],
                    self.variable_to_node[current_var],
                ));
            }
        }
        for combination in new_variables.iter().combinations(2) {
            self.graph.add_edge((
                self.variable_to_node[combination[0]],
                self.variable_to_node[combination[1]],
            ));
        }
    }

    fn update_live_set(
        &mut self,
        statement: &ast::Statement,
        mut current_live_set: LinkedHashSet<RcLocal>,
    ) -> LinkedHashSet<RcLocal> {
        let values_read = statement
            .values_read()
            .into_iter()
            .cloned()
            .collect::<LinkedHashSet<_>>();
        let new_variables = values_read
            .difference(&current_live_set)
            .cloned()
            .collect::<Vec<_>>();
        let removed_variables = statement
            .values_written()
            .into_iter()
            .cloned()
            .collect::<LinkedHashSet<_>>();
        let unused_variables = removed_variables
            .difference(&removed_variables)
            .cloned()
            .collect::<Vec<_>>();
        if !unused_variables.is_empty() {
            self.add_edges(&unused_variables, &current_live_set);
        }
        current_live_set = current_live_set
            .difference(&removed_variables)
            .cloned()
            .collect::<LinkedHashSet<_>>();
        if !new_variables.is_empty() {
            self.add_edges(&new_variables, &current_live_set);
        }
        current_live_set.extend(new_variables);
        current_live_set
    }

    pub fn new(function: &Function, liveness: &Liveness) -> Self {
        let mut this = InterferenceGraph {
            graph: Graph::new(),
            node_to_variable: FxHashMap::default(),
            variable_to_node: FxHashMap::default(),
        };
        for (node, block) in function.blocks() {
            this.create_interference(&liveness.live_in[node]);
            this.create_interference(&liveness.live_out[node]);
            let mut current_live_set = liveness.live_out[node].clone();
            for statement in block.iter().rev() {
                current_live_set = this.update_live_set(statement, current_live_set);
            }
            let live_in = &liveness.live_in[node];
            let dead_phis = liveness.defs_phi[node]
                .difference(&live_in)
                .cloned()
                .collect::<Vec<_>>();
            this.add_edges(&dead_phis, live_in);
        }
        this
    }
}
