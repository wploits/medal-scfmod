use ast::{LocalRw, RcLocal};
use rustc_hash::{FxHashMap, FxHashSet};

use petgraph::stable_graph::NodeIndex;

use crate::function::Function;

#[derive(Debug, Default)]
pub struct BlockLiveness {
    // the locals that are used in this block
    pub uses: FxHashSet<RcLocal>,
    // the locals that are defined in this block
    pub defs: FxHashSet<RcLocal>,
    // the locals that are used by arguments passed from this block to its successor
    pub arg_out_uses: FxHashSet<RcLocal>,
    // the locals that are defined by the parameters passed to this block by its predecessor
    pub params: FxHashSet<RcLocal>,
    // the set LiveIn(B) = PhiDefs(B) ⋃ ( [Uses(B) ⋃ LiveOut(B)] ∖ Defs(B))
    pub live_in: FxHashSet<RcLocal>,
    // the set LiveOut(B) = ( ⋃_{S ∊ Succ(B)} [LiveIn(S)∖PhiDefs(S)] ) ⋃ PhiUses(B)
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
        if block_liveness.params.contains(variable) {
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

    pub fn new(function: &Function, calc_live_sets: bool) -> Self {
        let mut liveness = Liveness {
            block_liveness: FxHashMap::with_capacity_and_hasher(
                function.graph().node_count(),
                Default::default(),
            ),
        };
        for (node, block) in function.blocks() {
            let block_liveness = liveness.block_liveness.entry(node).or_default();
            for instruction in block.iter() {
                block_liveness
                    .uses
                    .extend(instruction.values_read().into_iter().cloned());
                block_liveness
                    .defs
                    .extend(instruction.values_written().into_iter().cloned());
            }
            for (pred, edge) in function.edges_to_block(node) {
                liveness
                    .block_liveness
                    .get_mut(&node)
                    .unwrap()
                    .params
                    .extend(edge.arguments.iter().map(|(k, _)| k).cloned());
                let block_liveness = liveness.block_liveness.entry(pred).or_default();
                block_liveness.arg_out_uses.extend(
                    edge.arguments
                        .iter()
                        .flat_map(|(_, v)| v.values_read())
                        .cloned(),
                );
            }
        }
        if calc_live_sets {
            for node in function.graph().node_indices() {
                let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
                for variable in block_liveness.arg_out_uses.clone() {
                    let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
                    block_liveness.live_out.insert(variable.clone());
                    Self::explore_all_paths(&mut liveness, function, node, &variable);
                }
                let block_liveness = liveness.block_liveness.get_mut(&node).unwrap();
                for variable in block_liveness.uses.clone() {
                    Self::explore_all_paths(&mut liveness, function, node, &variable);
                }
            }
        }
        liveness
    }
}
