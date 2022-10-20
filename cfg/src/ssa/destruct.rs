use std::{cell::RefCell, rc::Rc, collections::{BTreeSet, BTreeMap}};

use ast::{RcLocal, LocalRw};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use petgraph::{algo::dominators::simple_fast, stable_graph::NodeIndex, visit::{EdgeRef, DfsPostOrder, Dfs}, Direction};

use crate::{function::Function, block::{BranchType, BlockEdge}};

mod interference_graph;
mod liveness;
mod local_declarations;
mod rename_locals;

use self::{
    interference_graph::InterferenceGraph, liveness::Liveness,
    rename_locals::LocalRenamer,
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
enum ParamOrStatIndex {
    Param(usize),
    Stat(usize),
}

// Benoit Boissinot, Alain Darte, Fabrice Rastello, Benoît Dupont de Dinechin, Christophe Guillon.
// Revisiting Out-of-SSA Translation for Correctness, Code Quality, and Eﬀiciency. [Research Report]
// 2008, pp.14. inria-00349925v3
// https://hal.inria.fr/inria-00349925/file/RR.pdf
// Slides: https://compilers.cs.uni-saarland.de/ssasem/talks/Alain.Darte.pdf
// https://github.com/LLVM-but-worse/maple-ir/blob/f8711230b7c63ce5fd916f86563912ec36f1217e/org.mapleir.ir/src/main/java/org/mapleir/ir/algorithms/BoissinotDestructor.java
pub struct Destructor<'a> {
    function: &'a mut Function,
    local_count: usize,
    values: FxHashMap<RcLocal, Rc<RefCell<FxHashSet<RcLocal>>>>,
    // map( local -> rc_map( local -> (pre-order block index, param index) ) )
    // TODO: hash map?
    congruence_classes: FxHashMap<RcLocal, Rc<RefCell<BTreeMap<(usize, ParamOrStatIndex), RcLocal>>>>, 
    local_defs: FxHashMap<RcLocal, (usize, ParamOrStatIndex)>,
}

impl<'a> Destructor<'a> {
    pub fn new(function: &'a mut Function, local_count: usize) -> Self {
        Self {
            function,
            local_count,
            values: FxHashMap::default(),
            congruence_classes: FxHashMap::default(),
            local_defs: FxHashMap::default(),
        }
    }
    pub fn destruct(mut self) -> IndexSet<RcLocal> {
        // TODO: remove detached blocks
        self.lift_params();
        self.sort_params();
        //crate::dot::render_to(self.function, &mut std::io::stdout()).unwrap();

        self.build_def_use();

        self.compute_value_interference();

        self.coalesce_params();
        self.coalesce_copies();

        todo!()
        
        // let liveness = Liveness::new(self.function);
        // let mut interference_graph = InterferenceGraph::new(self.function, &liveness, self.local_count);
        // ParamLifter::new(function, Some(&mut interference_graph)).lift();
        // let mut local_nodes = FxHashMap::default();
        // // interference graph is no longer chordal, coloring is not optimal
        // let upvalues = LocalRenamer::new(
        //     function,
        //     local_groups,
        //     upvalue_to_group,
        //     &mut interference_graph,
        //     &mut local_nodes,
        // )
        // .rename();
        // //crate::dot::render_to(function, &mut std::io::stdout()).unwrap();
        // let mut upvalues_in = upvalues;
        // upvalues_in.truncate(upvalues_in_count);
        // let dominators = simple_fast(function.graph(), function.entry().unwrap());
        // local_declarations::declare_locals(function, &upvalues_in, local_nodes, &dominators);
        // upvalues_in
    }

    // TODO: combine with compute value interference
    fn build_def_use(&mut self) {
        let mut dfs = Dfs::new(self.function.graph(), self.function.entry().unwrap());
        let mut dfs_index = 0;
        while let Some(node) = dfs.next(self.function.graph()) {
            if let Some((_, edge)) = self.function.edges_to_block(node).next() {
                for (param_index, (param, _)) in edge.arguments.iter().enumerate().collect::<Vec<_>>() {
                   assert!(self.local_defs.insert(param.clone(), (dfs_index, ParamOrStatIndex::Param(param_index))).is_none());
                }
            }
            for (stat_index, stat) in self.function.block(node).unwrap().0.iter().enumerate() {
                for local in stat.values_written() {
                    assert!(self.local_defs.insert(local.clone(), (dfs_index, ParamOrStatIndex::Stat(stat_index))).is_none());
                }
            }
            dfs_index += 1;
        }
    }

    // initialize congruence classes based on block params and remove block params
    fn coalesce_params(&mut self) {
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            for edge in self.function.graph().edges_directed(node, Direction::Incoming).map(|e| e.id()).collect::<Vec<_>>() {
                let args = std::mem::take(&mut self.function.graph_mut().edge_weight_mut(edge).unwrap().arguments);

                for (param, arg) in args {
                    let arg = arg.into_local().unwrap();
                    let congruence_class = self.congruence_classes.entry(param.clone()).or_insert_with(|| {
                        let mut congruence_class = BTreeMap::default();
                        congruence_class.insert(self.local_defs[&param], param);
                        Rc::new(RefCell::new(congruence_class))
                    }).clone();

                    congruence_class.borrow_mut().insert(self.local_defs[&arg], arg.clone());
                    self.congruence_classes.insert(arg, congruence_class);
                }
            }
        }
    }

    fn coalesce_copies(&mut self) {
        let mut dfs = Dfs::new(self.function.graph(), self.function.entry().unwrap());
        while let Some(node) = dfs.next(self.function.graph()) {
            self.function.block_mut(node).unwrap().retain(|stat| {
                if let ast::Statement::Assign(assign) = stat {
                    for (i, dst) in assign.left.iter().enumerate().filter_map(|(i, l)| Some((i, l.as_local()?))) {
                        todo!();
                    }
                }
                true
            })
        }
    }

    fn compute_value_interference(&mut self) {
        let mut dfs_post_order = DfsPostOrder::new(self.function.graph(), self.function.entry().unwrap());
        while let Some(node) = dfs_post_order.next(self.function.graph()) {
            if let Some((_, edge)) = self.function.edges_to_block(node).next() {
                for (p, _) in &edge.arguments {
                    assert!(self.values.insert(p.clone(), Default::default()).is_none());
                }
            }
            for stat in &self.function.block(node).unwrap().0 {
                if let ast::Statement::Assign(assign) = stat {
                    if assign.parallel {
                        for i in 0..assign.left.len() {
                            let dst = assign.left[i].clone().into_local().unwrap();
                            let src = assign.right[i].clone().into_local().unwrap();
                            let value_class = self.values.entry(src).or_default().clone();
                            value_class.borrow_mut().insert(dst.clone());
                            assert!(self.values.insert(dst, value_class).is_none());
                        }
                    } else {
                        for (i, dst) in assign.left.iter().enumerate().filter_map(|(i, l)| Some((i, l.as_local()?))) {
                            if let Some(ast::RValue::Local(src)) = assign.right.get(i) {
                                let value_class = self.values.entry(src.clone()).or_default().clone();
                                value_class.borrow_mut().insert(dst.clone());
                                assert!(self.values.insert(dst.clone(), value_class).is_none());  
                            } else {
                                assert!(self.values.insert(dst.clone(), Default::default()).is_none());
                            }
                        }
                    }
                }
            }
        }
    }

    fn sort_params(&mut self) {
        for edge in self.function.graph_mut().edge_weights_mut() {
            edge.arguments.sort_by(|(p0, _), (p1, _)| p0.cmp(p1));
        }
    }

    fn lift_params(&mut self) {
        for node in self.function.graph().node_indices().collect::<Vec<_>>() {
            self.lift_block_params(node);
        }
    }

    // Note that the phi-functions do not have a circular dependency and are ordered accordingly (we have to do this before),
    // i.e., no variable that is defined by a Phi-function is used in a 'later' phi-function.
    fn lift_block_params(&mut self, node: NodeIndex) {
        let local_allocator = self.function.local_allocator.clone();
        
        let mut param_map = FxHashMap::default();
        if let Some((_, BlockEdge { arguments, .. })) = self.function.edges_to_block(node).next() {
            for param in arguments.iter().map(|(p, _)| p) {
                param_map.insert(param.clone(), local_allocator.borrow_mut().allocate());
            }
        }

        if !param_map.is_empty() {
            self.function.block_mut(node).unwrap().insert(0, ast::Assign {
                left: param_map.keys().map(|k| k.clone().into()).collect(),
                right: param_map.values().map(|v| v.clone().into()).collect(),
                prefix: false,
                parallel: true,
            }.into());
        }

        // let mut new_nodes = FxHashSet::default();
        let mut preds = self.function.predecessor_blocks(node).detach();
        while let Some((_, pred)) = preds.next(self.function.graph()) {
            // if new_nodes.contains(&pred) {
            //     continue;
            // }

            let edges = self.function.edges(pred).collect::<Vec<_>>();
            let is_unconditional = edges.len() == 1;
            if is_unconditional {
                assert!(edges[0].weight().branch_type == BranchType::Unconditional);
            }
            let edges_to_node = edges
                .iter()
                .filter(|e| e.target() == node)
                .map(|e| e.id())
                .collect::<Vec<_>>();
            for &edge in &edges_to_node
            {
                let args = 
                    self
                        .function
                        .graph_mut()
                        .edge_weight_mut(edge)
                        .unwrap()
                        .arguments
                .iter_mut();

                let mut parallel_assign = ast::Assign { left: Vec::with_capacity(args.len()), right: Vec::with_capacity(args.len()), prefix: false, parallel: true, };
                for (param, arg) in args {
                    let temp_local = local_allocator.borrow_mut().allocate();

                    parallel_assign.left.push(temp_local.clone().into());
                    parallel_assign.right.push(arg.clone());
                    *param = param_map[param].clone();
                    *arg = temp_local.into();
                }

                if !parallel_assign.left.is_empty() {
                    let mut assign_block = pred;
                    // Benoit Boissinot, Alain Darte, Fabrice Rastello, Benoît Dupont de Dinechin, Christophe Guillon.
                    // Revisiting Out-of-SSA Translation for Correctness, Code Quality, and Eﬀiciency. [Research Report]
                    // 2008, pp.14. inria-00349925v3
                    // https://hal.inria.fr/inria-00349925/file/RR.pdf (p. 5)
                    // the condition of an if statement might write to an argument, resulting in interference
                    // we dont need to worry about instructions corresponding to other blocks being executed as we use
                    // temporary locals, so they will be discarded anyway.

                    // TODO: check values_written in the if statement
                    if !is_unconditional {
                        assert!(self.function.block(pred).unwrap().last().unwrap().values_written().is_empty());
                    }
                    // if !is_unconditional &&  {
                    //     assign_block = self.function.new_block();
                    //     let edge = self.function.graph_mut().remove_edge(edge).unwrap();
                    //     self.function.set_edges(
                    //         assign_block,
                    //         vec![(node, BlockEdge { branch_type: BranchType::Unconditional, arguments: edge.arguments })],
                    //     );
                        
                    //     self.function.graph_mut().add_edge(pred, assign_block, BlockEdge::new(edge.branch_type));
                    //     new_nodes.insert(assign_block);
                    // }

                    if is_unconditional {
                        self.function
                        .block_mut(assign_block)
                        .unwrap()
                        .push(parallel_assign.into());
                    } else {
                        let assign_block = self.function
                        .block_mut(assign_block)
                        .unwrap();
                        let len = assign_block.len();
                        assign_block.insert(len - 1, parallel_assign.into());
                    }
                }
            }
        }
    }
}
