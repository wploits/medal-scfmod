use std::io::Read;
use std::time;
use std::{fs::File, rc::Rc};

use clap::Parser;

use cfg_ir::{dot, ssa};
use lifter::Lifter;

mod chunk;
mod instruction;
mod lifter;
mod op_code;
mod value;

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

    let now = time::Instant::now();
    let chunk = chunk::parse(&buffer).unwrap().1;
    //println!("{:#?}", chunk);
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    let now = time::Instant::now();
    let mut cfg = Lifter::new(&chunk.main, Rc::default()).lift_function()?;
    let lifted = now.elapsed();
    dot::render_to(&cfg, &mut std::io::stdout())?;
    println!("lifting: {:?}", lifted);

    //let dfs = graph::algorithms::dfs_tree(graph, graph.entry().unwrap())?;
    //graph::dot::render_to(&dfs, &mut std::io::stdout())?;

    cfg_ir::value::ensure_write::ensure_writes(&mut cfg);

    let now = time::Instant::now();
    ssa::construct::construct(&mut cfg)?;
    let ssa_constructed = now.elapsed();
    dot::render_to(&cfg, &mut std::io::stdout())?;
    println!("ssa construction: {:?}", ssa_constructed);

    let now = time::Instant::now();
    ssa::destruct::destruct(&mut cfg);
    let ssa_destructed = now.elapsed();
    dot::render_to(&cfg, &mut std::io::stdout())?;
    println!("ssa destruction: {:?}", ssa_destructed);

    let now = time::Instant::now();
    let output = cfg_to_ast::lift(&cfg);
    let cfg_to_ast_time = now.elapsed();
    println!("cfg to ast lifter: {:?}", cfg_to_ast_time);

    println!("{}", output);

    Ok(())
}
