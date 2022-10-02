#![feature(box_patterns)]
#![feature(let_chains)]

use ast::name_locals::name_locals;
use cfg::ssa::structuring::{structure_for_loops, structure_method_calls};
use indexmap::IndexMap;
use restructure::post_dominators;
use std::{fs::File, io::Read, time};

use clap::Parser;

//use lifter::Lifter;
use cfg::{
    function::Function,
    inline::inline,
    ssa::structuring::{structure_compound_conditionals, structure_jumps},
};
use fxhash::FxHashMap;
use lua51_deserializer::chunk::Chunk;
use petgraph::{algo::dominators::simple_fast, stable_graph::NodeIndex};

mod lifter;

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    #[clap(short, long)]
    file: String,
}

fn main() -> anyhow::Result<()> {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    let args = Args::parse();

    let mut input = File::open(args.file)?;
    let mut buffer = vec![0; input.metadata()?.len() as usize];
    input.read_exact(&mut buffer)?;

    let total_now = time::Instant::now();
    let now = time::Instant::now();
    let chunk = Chunk::parse(&buffer).unwrap().1;
    //println!("{:#?}", chunk);
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    let now = time::Instant::now();
    //let function = Lifter::new(&chunk.function).lift_function()?;
    let (mut main, _, _) = lifter::LifterContext::lift(&chunk.function, Default::default());
    let lifted = now.elapsed();
    println!("lifting: {:?}", lifted);

    name_locals(&mut main, true);

    ast::formatter::Formatter::format(&main, &mut File::create("result.lua")?, Default::default())?;

    Ok(())
}
