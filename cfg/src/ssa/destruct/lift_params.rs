use fxhash::FxHashSet;
use petgraph::{stable_graph::NodeIndex, visit::EdgeRef, Direction};

use crate::{
    block::{BlockEdge, BranchType},
    function::Function,
};

use super::interference_graph::InterferenceGraph;

// https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/phi_lifting.py
pub struct ParamLifter<'a> {
    function: &'a mut Function,
    // we need to update interference graph as we go
    // this is required, as the graph can only be constructed before lifting
    interference_graph: Option<&'a mut InterferenceGraph>,
}

impl<'a> ParamLifter<'a> {
    pub fn new(
        function: &'a mut Function,
        interference_graph: Option<&'a mut InterferenceGraph>,
    ) -> Self {
        Self {
            function,
            interference_graph,
        }
    }

    pub fn lift(mut self) {
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            self.lift_params(node);
        }
    }

    // Note that the phi-functions do not have a circular dependency and are ordered accordingly (we have to do this before),
    // i.e., no variable that is defined by a Phi-function is used in a 'later' phi-function.
    fn lift_params(&mut self, node: NodeIndex) {
        let mut preds = self.function.predecessor_blocks(node).detach();
        while let Some((_, pred)) = preds.next(self.function.graph()) {
            let edges = self.function.edges(pred).collect::<Vec<_>>();
            let is_unconditional = edges.len() == 1;
            if is_unconditional {
                assert!(edges[0].weight().branch_type == BranchType::Unconditional);
            }
            let edges_to_node = edges
                .iter()
                .filter(|e| e.target() == node)
                .collect::<Vec<_>>();
            if edges_to_node.len() > 1 {
                todo!();
            }
            let edge = edges_to_node[0].id();
            let args = std::mem::take(
                &mut self
                    .function
                    .graph_mut()
                    .edge_weight_mut(edge)
                    .unwrap()
                    .arguments,
            )
            .into_iter()
            .filter(|(p, a)| p != a)
            .collect::<Vec<_>>();

            let mut assign_instrs = Vec::with_capacity(args.len());
            let mut defined_vars = FxHashSet::default();
            for (param, arg) in &args {
                // TODO: this check has performance implications, should we remove it?
                if defined_vars.contains(arg) {
                    panic!("block parameter lifting: arguments in incorrect order");
                }
                defined_vars.insert(param);

                assign_instrs.push(
                    ast::Assign::new(vec![(param.clone().into())], vec![arg.clone().into()]).into(),
                );
            }

            // update interference graph
            if let Some(interference_graph) = self.interference_graph.as_mut() {
                for (param, _) in &args {
                    let param_node = interference_graph.local_to_node[param];
                    for (_, arg) in &args {
                        interference_graph.graph.update_edge(
                            param_node,
                            interference_graph.local_to_node[arg],
                            (),
                        );
                    }
                }
            }

            // we dont want to end up creating a new block if there's nothing to add
            if !assign_instrs.is_empty() {
                let assign_block = if is_unconditional {
                    pred
                } else {
                    let new_block = self.function.new_block();
                    self.function.set_edges(
                        new_block,
                        vec![(node, BlockEdge::new(BranchType::Unconditional))],
                    );
                    // TODO: replace_edge
                    for edge in self
                        .function
                        .graph()
                        .edges_directed(pred, Direction::Outgoing)
                        .filter(|e| e.target() == node)
                        .map(|e| e.id())
                        .collect::<Vec<_>>()
                    {
                        let edge = self.function.graph_mut().remove_edge(edge).unwrap();
                        self.function.graph_mut().add_edge(pred, new_block, edge);
                    }
                    new_block
                };
                self.function
                    .block_mut(assign_block)
                    .unwrap()
                    .extend(assign_instrs);
            }
        }
    }
}
