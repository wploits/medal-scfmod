use ast::RcLocal;
use fxhash::{FxHashMap, FxHashSet};
use indexmap::{IndexMap, IndexSet};
use petgraph::algo::dominators::simple_fast;

use crate::function::Function;

mod interference_graph;
// mod lexicographical_bfs;
mod lift_params;
mod liveness;
mod local_declarations;
mod rename_locals;
mod resolve_param_dependencies;

use self::{
    interference_graph::InterferenceGraph, lift_params::ParamLifter, liveness::Liveness,
    rename_locals::LocalRenamer,
};

// TODO: use Sreedhar's algorithm, which will generate better output for weirdly obfuscated scripts
// based on https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/outofssatranslation.py
pub fn destruct(
    function: &mut Function,
    local_count: usize,
    local_groups: &[FxHashSet<RcLocal>],
    upvalue_to_group: &IndexMap<ast::RcLocal, usize>,
    upvalues_in_count: usize,
) -> IndexSet<RcLocal> {
    resolve_param_dependencies::resolve(function);
    let liveness = Liveness::new(function);
    let mut interference_graph = InterferenceGraph::new(function, &liveness, local_count);
    ParamLifter::new(function, Some(&mut interference_graph)).lift();
    let mut local_nodes = FxHashMap::default();
    // interference graph is no longer chordal, coloring is not optimal
    let upvalues = LocalRenamer::new(
        function,
        local_groups,
        upvalue_to_group,
        &mut interference_graph,
        &mut local_nodes,
    )
    .rename();
    //crate::dot::render_to(function, &mut std::io::stdout()).unwrap();
    let mut upvalues_in = upvalues;
    upvalues_in.truncate(upvalues_in_count);
    let dominators = simple_fast(function.graph(), function.entry().unwrap());
    local_declarations::declare_locals(function, &upvalues_in, local_nodes, &dominators);
    upvalues_in
}
