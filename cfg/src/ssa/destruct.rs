use std::time;

use ast::RcLocal;
use fxhash::{FxHashMap, FxHashSet};
use petgraph::algo::dominators::simple_fast;

use crate::function::Function;

mod interference_graph;
// mod lexicographical_bfs;
mod lift_params;
mod liveness;
mod local_declarations;
mod rename_variables;
mod resolve_param_dependencies;

use self::{
    interference_graph::InterferenceGraph, lift_params::ParamLifter, liveness::Liveness,
    rename_variables::VariableRenamer,
};

// TODO: use Sreedhar's algorithm, which will generate better output for weirdly obfuscated scripts
// based on https://github.com/fkie-cad/dewolf/blob/7afe5b46e79a7b56e9904e63f29d54bd8f7302d9/decompiler/pipeline/ssa/outofssatranslation.py
pub fn destruct(function: &mut Function, local_count: usize, local_groups: &[FxHashSet<RcLocal>]) {
    let now = time::Instant::now();
    resolve_param_dependencies::resolve(function);
    let elapsed = now.elapsed();
    println!("resolve param dependencies: {:?}", elapsed);

    let now = time::Instant::now();
    let liveness = Liveness::new(function);
    let elapsed = now.elapsed();
    println!("liveness: {:?}", elapsed);

    let now = time::Instant::now();
    let mut interference_graph = InterferenceGraph::new(function, &liveness, local_count);
    let elapsed = now.elapsed();
    println!("interference graph: {:?}", elapsed);

    let now = time::Instant::now();
    ParamLifter::new(function, Some(&mut interference_graph)).lift();
    let elapsed = now.elapsed();
    println!("param lifter: {:?}", elapsed);

    let mut local_nodes = FxHashMap::default();

    // interference graph is no longer chordal, coloring is not optimal
    let now = time::Instant::now();
    VariableRenamer::new(
        function,
        local_groups,
        &mut interference_graph,
        &mut local_nodes,
    )
    .rename();
    let elapsed = now.elapsed();
    println!("var renamer: {:?}", elapsed);

    let now = time::Instant::now();
    let dominators = simple_fast(function.graph(), function.entry().unwrap());
    local_declarations::declare_locals(function, local_nodes, &dominators);
    let elapsed = now.elapsed();
    println!("local declarations: {:?}", elapsed);
}
