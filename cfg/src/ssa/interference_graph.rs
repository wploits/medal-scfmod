use ast::RcLocal;
use fxhash::FxHashMap;
use graph::{Graph, NodeId, Undirected};
use linked_hash_set::LinkedHashSet;

use crate::function::Function;

use super::liveness::Liveness;


pub(crate) struct InterferenceGraph {
    graph: Graph<Undirected>,
    variables: FxHashMap<NodeId, RcLocal>
}

impl InterferenceGraph {
    fn create_interference(&mut self, variables: &LinkedHashSet<RcLocal>) {
        
    }

    fn add_edges(&mut self, new_variables: &[RcLocal], current_variables: &LinkedHashSet<RcLocal>) {

    }

    fn update_live_set(statement: &ast::Statement, live_set: &LinkedHashSet<RcLocal>) -> LinkedHashSet<RcLocal> {
        live_set.clone()
    }

    pub fn new(function: &Function, liveness: &Liveness) -> Self {
        let mut this = InterferenceGraph {
            graph: Graph::new(),
            variables: FxHashMap::default()
        };
        for (node, block) in function.blocks() {
            this.create_interference(&liveness.live_in[node]);
            this.create_interference(&liveness.live_out[node]);
            let mut current_live_set = liveness.live_out[node].clone();
            for statement in block.iter().rev() {
                current_live_set = Self::update_live_set(statement, &current_live_set);
            }
            let live_in = &liveness.live_in[node];
            let dead_phis = liveness.defs_phi[node].difference(&live_in).cloned().collect::<Vec<_>>();
            this.add_edges(&dead_phis, live_in);
        }
        this
    }
}