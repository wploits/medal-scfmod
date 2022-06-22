use std::{fs::File, io::Read, rc::Rc, time};

use clap::Parser;

use cfg_ir::function::Function;
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
    //let (mut main, _descendants) = Lifter::new(&chunk.main, Rc::default()).lift_function()?;
    let lifted = now.elapsed();

    // cfg_ir::new_ssa::construct(&mut main);

    /*process_function(&mut main)?;
    for function in descendants {
        process_function(&mut function.borrow_mut())?;
    }*/

   // dot::render_to(&main, &mut std::io::stdout())?;
    println!("lifting: {:?}", lifted);

    //let dfs = graph::algorithms::dfs_tree(graph, graph.entry().unwrap())?;
    //graph::dot::render_to(&dfs, &mut std::io::stdout())?;

    let now = time::Instant::now();
    //let output = cfg_to_ast::lift(&main);
    let cfg_to_ast_time = now.elapsed();
    println!("cfg to ast lifter: {:?}", cfg_to_ast_time);

    //println!("{}", output);

    Ok(())
}

/*fn process_function(cfg: &mut Function) -> anyhow::Result<()> {
    let now = time::Instant::now();
    cfg_ir::value::ensure_write::ensure_writes(cfg);
    let writes_ensured = now.elapsed();
    //dot::render_to(cfg, &mut std::io::stdout())?;
    println!("ensure writes: {:?}", writes_ensured);

    let now = time::Instant::now();
    ssa::construct::construct(cfg)?;
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