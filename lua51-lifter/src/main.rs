#![feature(box_patterns)]

use std::{fs::File, io::Read, time};

use clap::Parser;

//use lifter::Lifter;
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

    let now = time::Instant::now();
    let chunk = Chunk::parse(&buffer).unwrap().1;
    //println!("{:#?}", chunk);
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    let now = time::Instant::now();
    //let main = Lifter::new(&chunk.function).lift_function()?;
    let mut main = lifter::LifterContext::lift(&chunk.function);
    let lifted = now.elapsed();
    println!("lifting: {:?}", lifted);

    //cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let now = time::Instant::now();
    let (local_count, local_groups) = cfg::ssa::construct(&mut main);
    let ssa_constructed = now.elapsed();
    println!("ssa construction: {:?}", ssa_constructed);

    //cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let now = time::Instant::now();
    for node in main.graph().node_indices().collect::<Vec<_>>() {
        cfg::inline::inline_expressions(&mut main, node);
    }
    let inlined = now.elapsed();
    println!("inline: {:?}", inlined);

    //cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let now = time::Instant::now();
    cfg::ssa::destruct(&mut main, local_count, &local_groups);
    let ssa_destructed = now.elapsed();
    println!("ssa destruction: {:?}", ssa_destructed);

    //cfg::dot::render_to(&main, &mut std::io::stdout())?;

    /*process_function(&mut main)?;
    for function in descendants {
        process_function(&mut function.borrow_mut())?;
    }*/

    cfg::dot::render_to(&main, &mut std::io::stdout())?;
    let mut main = restructure::lift(main);

    let now = time::Instant::now();
    //ast::type_system::TypeSystem::analyze(&mut main);
    let type_analysis = now.elapsed();
    println!("type analysis: {:?}", type_analysis);

    let formatted = ast::formatter::Formatter::format(&main, Default::default());
    println!("{}", formatted);

    //let dfs = graph::algorithms::dfs_tree(graph, graph.entry().unwrap())?;
    // graph::dot::render_to(&main.graph(), &mut std::io::stdout())?;

    /*let now = time::Instant::now();
    let _output = cfg_to_ast_new::lift(main);
    let cfg_to_ast_time = now.elapsed();
    println!("cfg to ast lifter: {:?}", cfg_to_ast_time);*/

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
