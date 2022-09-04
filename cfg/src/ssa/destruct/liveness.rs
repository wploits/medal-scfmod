use ast::{LocalRw, RcLocal};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use petgraph::stable_graph::NodeIndex;

use crate::function::Function;

#[derive(Debug, Default)]
pub struct BlockLiveness {
    pub uses: FxHashSet<RcLocal>,
    pub defs: FxHashSet<RcLocal>,
    pub uses_phi: FxHashSet<RcLocal>,
    pub defs_phi: FxHashSet<RcLocal>,
    pub live_in: FxHashSet<RcLocal>,
    pub live_out: FxHashSet<RcLocal>,
}

#[derive(Debug)]
pub struct Liveness {
    pub block_liveness: FxHashMap<NodeIndex, BlockLiveness>,
}

impl Liveness {
    fn explore_all_paths(
        liveness: &mut Liveness,
        function: &Function,
        node: NodeIndex,
        variable: &RcLocal,
    ) {
        let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
        if block_liveness.defs.contains(variable) || block_liveness.live_in.contains(variable) {
            return;
        }
        block_liveness.live_in.insert(variable.clone());
        if block_liveness.defs_phi.contains(variable) {
            return;
        }
        for predecessor in function.predecessor_blocks(node) {
            liveness
                .block_liveness
                .get_mut(&predecessor)
                .unwrap()
                .live_out
                .insert(variable.clone());
            Self::explore_all_paths(liveness, function, predecessor, variable);
        }
    }

    pub fn new(function: &Function) -> Self {
        let mut liveness = Liveness {
            block_liveness: FxHashMap::with_capacity_and_hasher(function.graph().node_count(), Default::default()),
        };
        for (node, block) in function.blocks() {
            let block_liveness = liveness.block_liveness.entry(node).or_default();
            for instruction in block.ast.iter() {
                block_liveness
                    .uses
                    .extend(instruction.values_read().into_iter().cloned());
                block_liveness
                    .defs
                    .extend(instruction.values_written().into_iter().cloned());
            }
            for edge in function.edges_to_block(node) {
                liveness
                    .block_liveness
                    .entry(edge.node)
                    .or_default()
                    .defs_phi
                    .extend(edge.arguments.iter().map(|(k, _)| k).cloned());
                let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
                block_liveness
                    .uses_phi
                    .extend(edge.arguments.iter().map(|(_, v)| v).cloned());
            }
        }
        for node in function.graph().node_indices() {
            let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
            for variable in block_liveness.uses_phi.clone() {
                let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
                block_liveness.live_out.insert(variable.clone());
                Self::explore_all_paths(&mut liveness, function, node, &variable);
            }
            let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
            for variable in block_liveness.uses.clone() {
                Self::explore_all_paths(&mut liveness, function, node, &variable);
            }
        }
        liveness
    }
}
