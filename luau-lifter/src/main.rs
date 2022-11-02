mod deserializer;
mod instruction;
mod lifter;
mod op_code;
mod builtin_function;

use lifter::Lifter;

//use cfg_ir::{dot, function::Function, ssa};
use clap::Parser;
use std::{fs::File, io::Read, time};

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
        Bytecode::Error(_msg) => {
            println!("code did not compile");
        }
        Bytecode::Chunk(chunk) => {
            let now = time::Instant::now();
            let lifter = Lifter::new(&chunk.functions, &chunk.string_table, chunk.main);
            let main = lifter.lift_function()?;
            let lifted = now.elapsed();
            println!("lifting: {:?}", lifted);

            let result = restructure::lift(main);
            println!("{}", result);

            /*process_function(&mut main)?;
            for function in descendants.into_iter() {
                process_function(&mut function.borrow_mut())?;
            }*/

            //dot::render_to(&main, &mut std::io::stdout())?;

            /*let now = time::Instant::now();
            let output = cfg_to_ast::lift(&main);
            let cfg_to_ast_time = now.elapsed();
            println!("cfg to ast lifter: {:?}", cfg_to_ast_time);

            println!("{}", output);*/
        }
    }

    Ok(())
}

/*fn process_function(cfg: &mut Function) -> anyhow::Result<()> {
    let now = time::Instant::now();
    cfg_ir::value::ensure_write::ensure_writes(cfg);
    let writes_ensured = now.elapsed();
    //dot::render_to(cfg, &mut std::io::stdout())?;
    println!("ensure writes: {:?}", writes_ensured);

    let now = time::Instant::now();
    ssa::::construct(cfg)?;
    let ssa_constructed = now.elapsed();
    //dot::render_to(cfg, &mut std::io::stdout())?;
    println!("ssa construction: {:?}", ssa_constructed);

    let now = time::Instant::now();
    ssa::destruct::destruct(cfg);
    let ssa_destructed = now.elapsed();
    //dot::render_to(cfg, &mut std::io::stdout())?;
    println!("ssa destruction: {:?}", ssa_destructed);

    Ok(())
}
*/
