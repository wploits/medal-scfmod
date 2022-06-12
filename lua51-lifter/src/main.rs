mod chunk;
mod instruction;
mod lifter;
mod op_code;
mod value;

use cfg_ir::{dot, ssa};
use lifter::Lifter;

use clap::Parser;
use std::fs::File;
use std::io::Read;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    #[clap(short, long)]
    file: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut input = File::open(args.file)?;
    let mut buffer = vec![0; input.metadata()?.len() as usize];
    input.read_exact(&mut buffer)?;

    let chunk = chunk::parse(&buffer).unwrap().1;
    //println!("{:#?}", chunk);

    let mut cfg = Lifter::new(&chunk.main).lift_function()?;
    dot::render_to(&cfg, &mut std::io::stdout())?;

    //let dfs = graph::algorithms::dfs_tree(graph, graph.entry().unwrap())?;
    //graph::dot::render_to(&dfs, &mut std::io::stdout())?;

    ssa::construct::construct(&mut cfg)?;
    dot::render_to(&cfg, &mut std::io::stdout())?;

    //cfg_to_ast::lift(&cfg);

    Ok(())
}
