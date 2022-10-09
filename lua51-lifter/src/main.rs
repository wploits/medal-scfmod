#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(mixed_integer_ops)]

use ast::name_locals::name_locals;
use std::{
    fs::File,
    io::{Read, Write},
    time::Instant,
};

use clap::Parser;

use lua51_deserializer::chunk::Chunk;

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

    let start = Instant::now();
    let chunk = Chunk::parse(&buffer).unwrap().1;
    let (mut main, _, _) = lifter::LifterContext::lift(&chunk.function, Default::default());
    name_locals(&mut main, true);

    let res = main.to_string();
    let duration = start.elapsed();

    let mut out = File::create("result.lua")?;
    writeln!(out, "-- decompiled by Sentinel (took {:?})", duration)?;
    writeln!(out, "{}", res)?;

    Ok(())
}
