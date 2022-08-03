use ast::RcLocal;
use fxhash::FxHashMap;
use graph::NodeId;
use linked_hash_set::LinkedHashSet;
use ast::LocalRw;

use crate::function::Function;

// TODO: one hash map to a structure containing the sets
#[derive(Debug)]
pub struct Liveness {
    uses: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
    defs: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
    uses_phi: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
    defs_phi: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
    live_in: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
    live_out: FxHashMap<NodeId, LinkedHashSet<RcLocal>>,
}

impl Liveness {
    fn explore_all_paths(liveness: &mut Liveness, function: &Function, node: NodeId, variable: &RcLocal) {
        if liveness.defs[&node].contains(variable) || liveness.live_in[&node].contains(variable) {
            return;
        }
        liveness.live_in.get_mut(&node).unwrap().insert(variable.clone());
        if liveness.defs_phi[&node].contains(&variable) {
            for predecessor in function.graph().predecessors(node) {
                liveness.live_out.get_mut(&predecessor).unwrap().insert(variable.clone());
                Self::explore_all_paths(liveness, function, predecessor, variable);
            }
        }
    }

    pub fn new(function: &Function) -> Self {
        let mut liveness = Liveness {
            uses: FxHashMap::default(),
            defs: FxHashMap::default(),
            uses_phi: FxHashMap::default(),
            defs_phi: FxHashMap::default(),
            live_in: FxHashMap::default(),
            live_out: FxHashMap::default(),
        };
        for (&node, block) in function.blocks() {
            let uses = liveness.uses.entry(node).or_default();
            let defs = liveness.defs.entry(node).or_default();
            let uses_phi = liveness.uses_phi.entry(node).or_default();
            let defs_phi = liveness.defs_phi.entry(node).or_default();
            liveness.live_in.entry(node).or_default();
            liveness.live_out.entry(node).or_default();
            for edge in function.edges_to_block(node) {
                defs_phi.extend(edge.arguments.keys().cloned());
                uses_phi.extend(edge.arguments.values().cloned());
            }
            for instruction in block.iter() {
                uses.extend(instruction.values_read().into_iter().cloned());
                defs.extend(instruction.values_written().into_iter().cloned());
            }
        }
        for node in function.graph().nodes() {
            for variable in liveness.uses_phi[node].clone() {
                liveness.live_out.get_mut(node).unwrap().insert(variable.clone());
                Self::explore_all_paths(&mut liveness, function, *node, &variable);
            }
            for variable in liveness.uses[node].clone() {
                Self::explore_all_paths(&mut liveness, function, *node, &variable);
            }
        }
        liveness
    }
}