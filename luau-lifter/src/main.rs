mod deserializer;
mod instruction;
mod lifter;
mod op_code;

use ast::local_declarations::declare_locals;
use cfg::ssa::{
    self,
    structuring::{
        structure_compound_conditionals, structure_for_loops, structure_jumps,
        structure_method_calls,
    },
};
use indexmap::IndexMap;
use lifter::Lifter;

//use cfg_ir::{dot, function::Function, ssa};
use clap::Parser;
use petgraph::algo::dominators::simple_fast;
use restructure::post_dominators;
use rustc_hash::FxHashMap;
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
            let mut function = lifter.lift_function()?;
            let lifted = now.elapsed();
            println!("lifting: {:?}", lifted);

            let upvalues_in = Vec::new();
            let (local_count, local_groups, upvalue_in_groups, upvalue_passed_groups) =
                cfg::ssa::construct(&mut function, &upvalues_in);

            // println!("after ssa construction");
            // cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();

            let upvalue_to_group = upvalue_in_groups
                .into_iter()
                .chain(
                    upvalue_passed_groups
                        .into_iter()
                        .map(|m| (function.local_allocator.borrow_mut().allocate(), m)),
                )
                .flat_map(|(i, g)| g.into_iter().map(move |u| (u, i.clone())))
                .collect::<IndexMap<_, _>>();

            let local_to_group = local_groups
                .iter()
                .cloned()
                .enumerate()
                .flat_map(|(i, g)| g.into_iter().map(move |l| (l, i)))
                .collect::<FxHashMap<_, _>>();
            // TODO: REFACTOR: some way to write a macro that states
            // if cfg::ssa::inline results in change then structure_jumps, structure_compound_conditionals,
            // structure_for_loops and remove_unnecessary_params must run again.
            // if structure_compound_conditionals results in change then dominators and post dominators
            // must be recalculated.
            // etc.
            // the macro could also maybe generate an optimal ordering?
            let mut changed = true;
            while changed {
                changed = false;

                let dominators = simple_fast(function.graph(), function.entry().unwrap());
                changed |= structure_jumps(&mut function, &dominators);

                ssa::inline::inline(&mut function, &local_to_group, &upvalue_to_group);

                if structure_compound_conditionals(&mut function)
                    || {
                        let post_dominators = post_dominators(function.graph_mut());
                        structure_for_loops(&mut function, &dominators, &post_dominators)
                    }
                    || structure_method_calls(&mut function)
                {
                    changed = true;
                }

                // cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();

                let mut local_map = FxHashMap::default();

                // TODO: loop until returns false?
                if ssa::construct::remove_unnecessary_params(&mut function, &mut local_map) {
                    changed = true;
                }
                ssa::construct::apply_local_map(&mut function, local_map);
            }

            // let mut triangle_pattern_graph = PatternGraph::new();
            // let entry = triangle_pattern_graph.add_node(PatternNode::new(true));
            // let body = triangle_pattern_graph.add_node(PatternNode::new(false));
            // let exit = triangle_pattern_graph.add_node(PatternNode::new(true));

            // triangle_pattern_graph.add_edge(entry, body, BlockEdge::new(BranchType::Then));
            // triangle_pattern_graph.add_edge(entry, exit, BlockEdge::new(BranchType::Else));
            // triangle_pattern_graph.add_edge(body, exit, BlockEdge::new(BranchType::Unconditional));

            // println!(
            //     "triangle pattern: {}",
            //     Dot::with_config(&triangle_pattern_graph, &[])
            // );

            // panic!();

            // cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();
            //let dataflow = cfg::ssa::dataflow::DataFlow::new(&function);
            //println!("dataflow: {:#?}", dataflow);

            //cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();

            cfg::ssa::Destructor::new(
                &mut function,
                &upvalue_to_group,
                upvalues_in.iter().cloned().collect(),
                local_count,
            )
            .destruct();

            let params = std::mem::take(&mut function.parameters);
            let mut block = restructure::lift(function);
            declare_locals(
                &mut block,
                &upvalues_in.iter().chain(params.iter()).cloned().collect(),
            );
            println!("{}", block);

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
