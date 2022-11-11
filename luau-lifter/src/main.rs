#![feature(is_some_and)]

mod deserializer;
mod instruction;
mod lifter;
mod op_code;

use ast::{local_declarations::declare_locals, name_locals::name_locals};
use cfg::ssa::{
    self,
    structuring::{
        structure_conditionals, structure_for_loops, structure_jumps, structure_method_calls,
    },
};
use indexmap::IndexMap;
use lifter::Lifter;

//use cfg_ir::{dot, function::Function, ssa};
use clap::Parser;
use petgraph::algo::dominators::simple_fast;
use restructure::post_dominators;
use rustc_hash::FxHashMap;
use std::{
    fs::File,
    io::{Read, Write},
    time::{self, Instant}, path::Path,
};

use deserializer::bytecode::Bytecode;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    #[clap(short, long)]
    file: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let path = Path::new(&args.file);
    let mut input = File::open(path)?;
    let mut buffer = vec![0; input.metadata()?.len() as usize];
    input.read_exact(&mut buffer)?;

    let now = time::Instant::now();
    let chunk = deserializer::deserialize(&buffer, false).unwrap();
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    match chunk {
        Bytecode::Error(_msg) => {
            println!("code did not compile");
        }
        Bytecode::Chunk(chunk) => {
            let start = Instant::now();
            let (mut main, _, _) = Lifter::lift(
                &chunk.functions,
                &chunk.string_table,
                chunk.main,
                Default::default(),
            );
            name_locals(&mut main, true);

            let res = main.to_string();
            let duration = start.elapsed();

            let mut out = File::create(path.with_extension("dec.lua"))?;
            writeln!(out, "-- decompiled by Sentinel (took {:?})", duration)?;
            writeln!(out, "{}", res)?;
        }
    }

    Ok(())
}
