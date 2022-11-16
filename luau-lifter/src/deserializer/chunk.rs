use super::{function::Function, list::parse_list, parse_string};
use nom::IResult;
use nom_leb128::leb128_usize;

#[derive(Debug)]
pub struct Chunk {
    pub string_table: Vec<Vec<u8>>,
    pub functions: Vec<Function>,
    pub main: usize,
}

impl Chunk {
    pub(crate) fn parse(input: &[u8], use_rbx_encoding: bool) -> IResult<&[u8], Self> {
        let (input, string_table) = parse_list(input, parse_string)?;
        let (input, functions) = parse_list(input, |i| Function::parse(i, use_rbx_encoding))?;
        let (input, main) = leb128_usize(input)?;

        Ok((
            input,
            Self {
                string_table,
                functions,
                main,
            },
        ))
    }
}
