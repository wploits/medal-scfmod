use nom::{bytes::complete::take, IResult};
use nom_leb128::leb128_usize;

pub mod bytecode;
pub mod chunk;
pub mod constant;
pub mod function;
mod list;

fn parse_string(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = leb128_usize(input)?;
    let (input, bytes) = take(length)(input)?;
    let string = String::from_utf8_lossy(bytes).to_string();
    Ok((input, string))
}

pub fn deserialize(bytecode: &[u8]) -> Result<bytecode::Bytecode, String> {
    match bytecode::Bytecode::parse(bytecode) {
        Ok((_, deserialized_bytecode)) => Ok(deserialized_bytecode),
        Err(err) => Err(err.to_string()),
    }
}

/*#[test]
fn main() -> anyhow::Result<()> {
    let compiler = Compiler::new()
        .set_debug_level(1).set_optimization_level(2);
    let bytecode = compiler.compile("asd = test");
    println!("{:#?}", bytecode);
    let deserialized = deserialize(&bytecode);
    println!("{:#?}", deserialized);
    Ok(())
}*/
