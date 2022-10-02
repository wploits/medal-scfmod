#![feature(box_patterns)]
#![feature(let_chains)]

use ast::{structure_functions::structure_functions, name_locals::name_locals};
use cfg::ssa::structuring::structure_for_loops;
use indexmap::IndexMap;
use restructure::post_dominators;
use std::{fs::File, io::Read, time};

use clap::Parser;

//use lifter::Lifter;
use cfg::{
    function::Function,
    inline::inline,
    ssa::structuring::{structure_compound_conditionals, structure_jumps},
};
use fxhash::FxHashMap;
use lua51_deserializer::chunk::Chunk;
use petgraph::{algo::dominators::simple_fast, stable_graph::NodeIndex};

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

    let total_now = time::Instant::now();
    let now = time::Instant::now();
    let chunk = Chunk::parse(&buffer).unwrap().1;
    //println!("{:#?}", chunk);
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    let now = time::Instant::now();
    //let function = Lifter::new(&chunk.function).lift_function()?;
    let lifted_functions =
        lifter::LifterContext::lift(&chunk.function, Default::default(), Vec::new());
    let lifted = now.elapsed();
    println!("lifting: {:?}", lifted);

    let mut structured_functions = Vec::new();
    for (mut function, upvalues_in) in lifted_functions {
        cfg::dot::render_to(&function, &mut std::io::stdout())?;

        let now = time::Instant::now();
        let (local_count, local_groups, upvalue_groups) =
            cfg::ssa::construct(&mut function, &upvalues_in);
        let ssa_constructed = now.elapsed();
        println!("ssa construction: {:?}", ssa_constructed);

        cfg::dot::render_to(&function, &mut std::io::stdout())?;

        let upvalue_to_group = upvalue_groups
            .iter()
            .cloned()
            .enumerate()
            .flat_map(|(i, g)| g.into_iter().map(move |u| (u, i)))
            .collect::<IndexMap<_, _>>();

        println!("uv groups: {:?}", upvalue_groups);

        let mut iterations = 0;
        let now = time::Instant::now();
        loop {
            iterations += 1;

            // TODO: remove unused variables without side effects
            inline(&mut function, &upvalue_to_group);

            let mut dominators = None;
            if !structure_compound_conditionals(&mut function)
                && !{
                    dominators = Some(simple_fast(function.graph(), function.entry().unwrap()));
                    let post_dominators = post_dominators(function.graph().clone());
                    structure_for_loops(
                        &mut function,
                        dominators.as_ref().unwrap(),
                        &post_dominators,
                    )
                }
            {
                break;
            }

            let mut local_map = FxHashMap::default();
            cfg::ssa::construct::remove_unnecessary_params(&mut function, &mut local_map);
            cfg::ssa::construct::apply_local_map(&mut function, local_map);

            if dominators.is_none() {
                dominators = Some(simple_fast(function.graph(), function.entry().unwrap()));
            }
            structure_jumps(&mut function, dominators.as_ref().unwrap());
        }
        let inlined = now.elapsed();
        println!(
            "inlining and structuring (looped {} times): {:?}",
            iterations, inlined
        );

        cfg::dot::render_to(&function, &mut std::io::stdout())?;

        //let dataflow = cfg::ssa::dataflow::DataFlow::new(&function);
        //println!("dataflow: {:#?}", dataflow);

        //cfg::dot::render_to(&function, &mut std::io::stdout())?;

        let now = time::Instant::now();
        let upvalues_in = cfg::ssa::destruct(
            &mut function,
            local_count,
            &local_groups,
            &upvalue_to_group,
            upvalues_in.len(),
        )
        .into_iter()
        .collect::<Vec<_>>();
        let ssa_destructed = now.elapsed();
        println!("ssa destruction: {:?}", ssa_destructed);

        cfg::dot::render_to(&function, &mut std::io::stdout())?;

        /*process_function(&mut function)?;
        for function in descendants {
            process_function(&mut function.borrow_mut())?;
        }*/

        // cfg::dot::render_to(&function, &mut std::io::stdout())?;
        let params = function.parameters.clone();
        structured_functions.push((restructure::lift(function), params, upvalues_in));
    }
    
    let mut main = structure_functions(structured_functions);

    let now = time::Instant::now();
    //ast::type_system::TypeSystem::analyze(&mut function);
    let _type_analysis = now.elapsed();
    //println!("type analysis: {:?}", type_analysis);

    name_locals(&mut main, true);

    let formatted = ast::formatter::Formatter::format(&main, Default::default());
    println!("{}", formatted);

    println!("total time: {:?}", total_now.elapsed());

    //let dfs = graph::algorithms::dfs_tree(graph, graph.entry().unwrap())?;
    // graph::dot::render_to(&function.graph(), &mut std::io::stdout())?;

    /*let now = time::Instant::now();
    let _output = cfg_to_ast_new::lift(function);
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
