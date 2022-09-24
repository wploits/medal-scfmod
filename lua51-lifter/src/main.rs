#![feature(box_patterns)]

use cfg::ssa::structuring::structure_for_loops;
use restructure::post_dominators;
use std::{fs::File, io::Read, time};

use clap::Parser;

//use lifter::Lifter;
use cfg::{
    function::Function,
    inline::inline_expressions,
    ssa::structuring::{structure_compound_conditionals, structure_jumps},
};
use fxhash::FxHashMap;
use lua51_deserializer::chunk::Chunk;
use petgraph::algo::dominators::simple_fast;

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

pub fn inline_setlists(function: &mut Function) {
    let mut changed = true;
    while changed {
        inline_expressions(function);
        changed = false;
        'outer: for (_, block) in function.blocks_mut() {
            let mut i = 0;
            while i < block.ast.len() {
                if let ast::Statement::SetList(setlist) = block.ast[i].clone() {
                    if let Some(assign)= block.ast.get_mut(i - 1).unwrap().as_assign_mut() && assign.left == vec![setlist.table.into()] {
                        let table = assign.right[0].as_table_mut().unwrap();
                        assert!(table.0.len() == setlist.index - 1);
                        for value in setlist.values {
                            table.0.push(value);
                        }
                        if let Some(tail) = setlist.tail {
                            table.0.push(tail);
                        }
                        block.ast.remove(i);
                        changed = true;
                    } else {
                        i += 1;
                    }
                    // todo: only inline in changed blocks
                    //cfg::dot::render_to(function, &mut std::io::stdout());
                    //break 'outer;
                } else {
                    i += 1;
                }
            }
        }
    }
}

fn main() -> anyhow::Result<()> {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::new_heap();

    let args = Args::parse();

    let mut input = File::open(args.file)?;
    let mut buffer = vec![0; input.metadata()?.len() as usize];
    input.read_exact(&mut buffer)?;

    let total_now = time::Instant::now();
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

    cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let now = time::Instant::now();
    let (local_count, local_groups) = cfg::ssa::construct(&mut main);
    let ssa_constructed = now.elapsed();
    println!("ssa construction: {:?}", ssa_constructed);

    cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let mut iterations = 0;
    let now = time::Instant::now();
    loop {
        iterations += 1;

        // TODO: remove unused variables without side effects
        cfg::inline::inline_expressions(&mut main);
        inline_setlists(&mut main);

        let mut dominators = None;
        if !structure_compound_conditionals(&mut main)
            && !{
                dominators = Some(simple_fast(main.graph(), main.entry().unwrap()));
                let post_dominators = post_dominators(main.graph().clone());
                structure_for_loops(&mut main, dominators.as_ref().unwrap(), &post_dominators)
            }
        {
            break;
        }

        let mut local_map = FxHashMap::default();
        cfg::ssa::construct::remove_unnecessary_params(&mut main, &mut local_map);
        cfg::ssa::construct::apply_local_map(&mut main, local_map);

        if dominators.is_none() {
            dominators = Some(simple_fast(main.graph(), main.entry().unwrap()));
        }
        structure_jumps(&mut main, dominators.as_ref().unwrap());
    }
    let inlined = now.elapsed();
    println!(
        "inlining and structuring (looped {} times): {:?}",
        iterations, inlined
    );

    cfg::dot::render_to(&main, &mut std::io::stdout())?;

    //let dataflow = cfg::ssa::dataflow::DataFlow::new(&main);
    //println!("dataflow: {:#?}", dataflow);

    //cfg::dot::render_to(&main, &mut std::io::stdout())?;

    let now = time::Instant::now();
    cfg::ssa::destruct(&mut main, local_count, &local_groups);
    let ssa_destructed = now.elapsed();
    println!("ssa destruction: {:?}", ssa_destructed);

    cfg::dot::render_to(&main, &mut std::io::stdout())?;

    /*process_function(&mut main)?;
    for function in descendants {
        process_function(&mut function.borrow_mut())?;
    }*/

    // cfg::dot::render_to(&main, &mut std::io::stdout())?;
    let main = restructure::lift(main);

    let now = time::Instant::now();
    //ast::type_system::TypeSystem::analyze(&mut main);
    let _type_analysis = now.elapsed();
    //println!("type analysis: {:?}", type_analysis);

    let formatted = ast::formatter::Formatter::format(&main, Default::default());
    println!("{}", formatted);

    println!("total time: {:?}", total_now.elapsed());

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
