mod deserializer;
mod instruction;
mod lifter;
mod op_code;

use lifter::Lifter;

use clap::Parser;
use std::fs::File;
use std::io::Read;
use cfg_ir::ssa;
use std::time;

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

    let now = time::Instant::now();
    let chunk = deserializer::compile(&String::from_utf8(buffer).unwrap()).unwrap();
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);
    
    match chunk {
        Bytecode::Error(msg) => {
            println!("code did not compile");
        }
        Bytecode::Chunk(chunk) => {
            let now = time::Instant::now();
            let mut cfg = Lifter::new(&chunk.functions, &chunk.string_table, chunk.main).lift_function()?;
            let lifted = now.elapsed();
            println!("lifting: {:?}", lifted);

            //let graph = cfg.graph();
            //graph::dot::render_to(graph, &mut std::io::stdout())?;

            let now = time::Instant::now();
            ssa::construct::construct(&mut cfg)?;
            let ssa_constructed = now.elapsed();
            //dot::render_to(&cfg, &mut std::io::stdout())?;
            println!("ssa construction: {:?}", ssa_constructed);

            let now = time::Instant::now();
            ssa::destruct::destruct(&mut cfg);
            let ssa_destructed = now.elapsed();
            //dot::render_to(&cfg, &mut std::io::stdout())?;
            println!("ssa destruction: {:?}", ssa_destructed);

            //println!("beginning lift");
            let ast = cfg_to_ast::lift(&cfg);
            //println!("{:#?}", ast);
        }
    }

    Ok(())
}
