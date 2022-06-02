mod deserializer;
mod instruction;
mod lifter;
mod op_code;

use lifter::Lifter;

use clap::Parser;
use std::fs::File;
use std::io::Read;

use deserializer::bytecode::Bytecode;

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

    let chunk = deserializer::compile(&String::from("while a true do end")).unwrap();

    match chunk {
        Bytecode::Error(msg) => {
            println!("code did not compile");
        }
        Bytecode::Chunk(chunk) => {
            let cfg = Lifter::new(chunk).lift_function()?;
            let graph = cfg.graph();
            graph::dot::render_to(graph, &mut std::io::stdout())?;

            println!("beginning lift");
            let ast = cfg_to_ast::lift(&cfg);
            //println!("{:#?}", ast);
        }
    }

    Ok(())
}
