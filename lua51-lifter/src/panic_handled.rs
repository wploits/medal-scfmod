#![feature(box_patterns)]
#![feature(let_chains)]

use ast::structure_functions::structure_functions;
use cfg::{ssa::structuring::structure_for_loops, inline::inline};
use indexmap::IndexMap;
use restructure::post_dominators;
use std::{fs::File, io::Read, time};

use clap::Parser;

//use lifter::Lifter;
use cfg::{
    function::Function,
    ssa::structuring::{structure_compound_conditionals, structure_jumps},
};
use fxhash::{FxHashMap, FxHashSet};
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
        crate::lifter::LifterContext::lift(&chunk.function, Default::default(), Vec::new());
    let lifted = now.elapsed();
    println!("lifting: {:?}", lifted);

    let mut times = Vec::new();
    let mut structured_functions = Vec::new();
    for (function, upvalues_in) in lifted_functions {
        let mut wrapper = std::panic::AssertUnwindSafe(Some(function));
        let mut wrapper2 = std::panic::AssertUnwindSafe(Some(upvalues_in));
        let func_start_instant = time::Instant::now();
        let result = std::panic::catch_unwind(move || {
            let (mut function, upvalues_in) = (wrapper.take().unwrap(), wrapper2.take().unwrap());
            let (local_count, local_groups, upvalue_groups) =
                cfg::ssa::construct(&mut function, &upvalues_in);

            //cfg::dot::render_to(&function, &mut std::io::stdout())?;

            let upvalue_to_group = upvalue_groups
                .iter()
                .cloned()
                .enumerate()
                .flat_map(|(i, g)| g.into_iter().map(move |u| (u, i)))
                .collect::<IndexMap<_, _>>();

            //let now = time::Instant::now();
            loop {
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
            // let inlined = now.elapsed();
            // println!(
            //     "inlining and structuring (looped {} times): {:?}",
            //     iterations, inlined
            // );

            //cfg::dot::render_to(&function, &mut std::io::stdout())?;

            //let dataflow = cfg::ssa::dataflow::DataFlow::new(&function);
            //println!("dataflow: {:#?}", dataflow);

            //cfg::dot::render_to(&function, &mut std::io::stdout())?;

            let upvalues_in = cfg::ssa::destruct(
                &mut function,
                local_count,
                &local_groups,
                &upvalue_to_group,
                upvalues_in.len(),
            )
            .into_iter()
            .collect::<Vec<_>>();

            // cfg::dot::render_to(&function, &mut std::io::stdout())?;
            let params = function.parameters.clone();
            (restructure::lift(function.clone()), params, upvalues_in)
        });
        times.push(func_start_instant.elapsed());

        structured_functions.push(match result {
            Ok(r) => r,
            Err(e) => {
                let panic_information = match e.downcast::<String>() {
                    Ok(v) => *v,
                    Err(e) => match e.downcast::<&str>() {
                        Ok(v) => v.to_string(),
                        _ => "Unknown Source of Error".to_owned(),
                    },
                };

                let mut block = ast::Block::from_vec(vec![ast::Comment::new(
                    "the decompiler panicked while decompiling this function".to_string(),
                )
                .into()]);

                for line in panic_information.split('\n') {
                    block.push(ast::Comment::new(line.to_string()).into());
                }

                (block, Vec::new(), Vec::new())
            }
        });
    }

    let main = structure_functions(structured_functions);

    let now = time::Instant::now();
    //ast::type_system::TypeSystem::analyze(&mut function);
    let _type_analysis = now.elapsed();
    //println!("type analysis: {:?}", type_analysis);

    let formatted = ast::formatter::Formatter::format(&main, Default::default());
    std::fs::write("result.lua", formatted).unwrap();

    println!(
        "longest func time: {:?}",
        times
            .into_iter()
            .fold(time::Duration::ZERO, |a, b| a.max(b))
    );
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
