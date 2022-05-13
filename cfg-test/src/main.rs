use anyhow::Result;

use cfg_ir::{builder::Builder, function::Function, instruction::UnconditionalJump};

fn main() -> Result<()> {
    let mut function = Function::new();
    let mut builder = Builder::new(&mut function);

    let block = builder.new_block()?.block_id();
    builder
        .new_block()?
        .mark_entry()
        .replace_terminator(UnconditionalJump(block).into())?;
    builder
        .block(block)?
        .replace_terminator(UnconditionalJump(block).into())?;

    let graph = function.graph();
    let entry = graph.entry().unwrap();
    println!("entry: {}", entry);

    println!("immediate doms: {:#?}", graph.compute_immediate_dominators(entry));

    //let dominance_frontiers = graph.compute_dominance_frontiers(entry, None);
    //println!("{:#?}", dominance_frontiers);

    graph::dot::render_to(function.graph(), &mut std::io::stdout())?;

    Ok(())
}
