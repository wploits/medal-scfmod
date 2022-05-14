mod chunk;
mod instruction;
mod lifter;
mod op_code;
mod value;

use cfg_ir::error::Error;
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

    let ir_function = Lifter::new(&chunk.main).lift_function()?;
    let graph = ir_function.graph();

    graph::dot::render_to(graph, &mut std::io::stdout())?;

    println!(
        "immediate doms: {:#?}",
        graph::algorithms::dominators::compute_immediate_dominators(
            graph,
            graph.entry().ok_or(graph::Error::NoEntry)?
        )?
    );

    graph::algorithms::flow::test(graph)?;

    /*let flow_info = cfg_ir::function::flow_info::FlowInfo::new(ir_function.graph_mut())?;
    println!("{:#?}", flow_info);*/

    Ok(())
}
