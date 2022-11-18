#![feature(is_some_and)]

mod deserializer;
mod instruction;
mod lifter;
mod op_code;

use ast::{
    local_declarations::LocalDeclarer, name_locals::name_locals, replace_locals::replace_locals,
    Traverse,
};

use by_address::ByAddress;
use cfg::ssa::{
    self,
    structuring::{structure_conditionals, structure_for_loops, structure_jumps},
};
use indexmap::IndexMap;

use lifter::Lifter;

//use cfg_ir::{dot, function::Function, ssa};
use clap::Parser;
use petgraph::algo::dominators::simple_fast;
use rayon::prelude::*;
use restructure::post_dominators;
use rustc_hash::FxHashMap;
use triomphe::Arc;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
    sync::{Mutex},
    time::{self, Instant},
};

use deserializer::bytecode::Bytecode;

#[global_allocator]
static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

#[derive(Parser, Debug)]
#[clap(about, version, author)]
struct Args {
    // File path
    #[clap(short, long)]
    file: String,
    // Number of threads to use (0 = use Rayon default)
    #[clap(short, long, default_value_t = 0)]
    threads: usize,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    rayon::ThreadPoolBuilder::new()
        .num_threads(args.threads)
        .build_global()
        .unwrap();

    let path = Path::new(&args.file);
    let mut input = File::open(path)?;
    let mut buffer = vec![0; input.metadata()?.len() as usize];
    input.read_exact(&mut buffer)?;

    let now = time::Instant::now();
    let chunk = deserializer::deserialize(&buffer, false).unwrap();
    let parsed = now.elapsed();
    println!("parsing: {:?}", parsed);

    match chunk {
        Bytecode::Error(_msg) => {
            println!("code did not compile");
        }
        Bytecode::Chunk(chunk) => {
            let start = Instant::now();
            let mut lifted = Vec::new();
            let mut stack = vec![(Arc::<Mutex<ast::Function>>::default(), chunk.main)];
            while let Some((ast_func, func_id)) = stack.pop() {
                let (function, upvalues, child_functions) =
                    Lifter::lift(&chunk.functions, &chunk.string_table, func_id);
                lifted.push((ast_func, function, upvalues));
                stack.extend(child_functions.into_iter().map(|(a, f)| (a.0, f)));
            }

            let (main, ..) = lifted.first().unwrap().clone();
            let mut upvalues = lifted
                .into_par_iter()
                .map(|(ast_function, mut function, upvalues_in)| {
                    let (local_count, local_groups, upvalue_in_groups, upvalue_passed_groups) =
                        cfg::ssa::construct(&mut function, &upvalues_in);
                    let upvalue_to_group = upvalue_in_groups
                        .into_iter()
                        .chain(
                            upvalue_passed_groups
                                .into_iter()
                                .map(|m| (ast::RcLocal::default(), m)),
                        )
                        .flat_map(|(i, g)| g.into_iter().map(move |u| (u, i.clone())))
                        .collect::<IndexMap<_, _>>();
                    // TODO: do we even need this?
                    let local_to_group = local_groups
                        .into_iter()
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

                        if structure_conditionals(&mut function) || {
                            let post_dominators = post_dominators(function.graph_mut());
                            structure_for_loops(&mut function, &dominators, &post_dominators)
                        }
                        // we can't structure method calls like this because of __namecall
                        // || structure_method_calls(&mut function)
                        {
                            changed = true;
                        }
                        let mut local_map = FxHashMap::default();
                        // TODO: loop until returns false?
                        if ssa::construct::remove_unnecessary_params(&mut function, &mut local_map)
                        {
                            changed = true;
                        }
                        ssa::construct::apply_local_map(&mut function, local_map);
                    }
                    ssa::Destructor::new(
                        &mut function,
                        upvalue_to_group,
                        upvalues_in.iter().cloned().collect(),
                        local_count,
                    )
                    .destruct();

                    let params = std::mem::take(&mut function.parameters);
                    let is_variadic = function.is_variadic;
                    let block = Arc::new(restructure::lift(function).into());
                    LocalDeclarer::default().declare_locals(
                        // TODO: why does block.clone() not work?
                        Arc::clone(&block),
                        &upvalues_in.iter().chain(params.iter()).cloned().collect(),
                    );

                    {
                        let mut ast_function = ast_function.lock().unwrap();
                        ast_function.body = Arc::try_unwrap(block)
                            .unwrap()
                            .into_inner()
                            .unwrap();
                        ast_function.parameters = params;
                        ast_function.is_variadic = is_variadic;
                    }
                    (ByAddress(ast_function), upvalues_in)
                })
                .collect::<FxHashMap<_, _>>();

            let main = ByAddress(main);
            upvalues.remove(&main);
            let mut body = Arc::try_unwrap(main.0)
                .unwrap()
                .into_inner()
                .unwrap()
                .body;
            link_upvalues(&mut body, &mut upvalues);
            name_locals(&mut body, true);
            let res = body.to_string();
            let duration = start.elapsed();

            // TODO: use BufWriter?
            let mut out = File::create(path.with_extension("dec.lua"))?;
            writeln!(out, "-- decompiled by Sentinel (took {:?})", duration)?;
            writeln!(out, "{}", res)?;
        }
    }

    Ok(())
}

fn link_upvalues(
    body: &mut ast::Block,
    upvalues: &mut FxHashMap<
        ByAddress<Arc<Mutex<ast::Function>>>,
        Vec<ast::RcLocal>,
    >,
) {
    for stat in &mut body.0 {
        stat.traverse_rvalues(&mut |rvalue| {
            if let ast::RValue::Closure(closure) = rvalue {
                let old_upvalues = upvalues.remove(&closure.function).unwrap();
                let mut function = closure.function.lock().unwrap();
                // TODO: inefficient, try constructing a map of all up -> new up first
                // and then call replace_locals on main body
                let mut local_map = FxHashMap::with_capacity_and_hasher(
                    old_upvalues.len(),
                    Default::default(),
                );
                for (old, new) in
                    old_upvalues
                        .iter()
                        .zip(closure.upvalues.iter().map(|u| match u {
                            ast::Upvalue::Copy(l) | ast::Upvalue::Ref(l) => l,
                        }))
                {
                    // println!("{} -> {}", old, new);
                    local_map.insert(old.clone(), new.clone());
                }
                link_upvalues(&mut function.body, upvalues);
                replace_locals(&mut function.body, &local_map);
            }
        });
        match stat {
            ast::Statement::If(r#if) => {
                link_upvalues(&mut r#if.then_block.lock().unwrap(), upvalues);
                link_upvalues(&mut r#if.else_block.lock().unwrap(), upvalues);
            }
            ast::Statement::While(r#while) => {
                link_upvalues(&mut r#while.block.lock().unwrap(), upvalues);
            }
            ast::Statement::Repeat(repeat) => {
                link_upvalues(&mut repeat.block.lock().unwrap(), upvalues);
            }
            ast::Statement::NumericFor(numeric_for) => {
                link_upvalues(&mut numeric_for.block.lock().unwrap(), upvalues);
            }
            ast::Statement::GenericFor(generic_for) => {
                link_upvalues(&mut generic_for.block.lock().unwrap(), upvalues);
            }
            _ => {}
        }
    }
}