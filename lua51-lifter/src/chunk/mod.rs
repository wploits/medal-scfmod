use nom::*;

use crate::value::parse_str;

use function::Function;
use header::Header;

pub mod function;
pub mod header;

#[derive(Debug)]
pub struct Chunk<'a> {
    pub header: Header,
    pub main: Function<'a>,
}

pub(crate) fn parse(input: &[u8]) -> IResult<&[u8], Chunk> {
    let (input, header) = Header::parse(input)?;
    let (input, main) = Function::parse(input)?;

    Ok((
        input,
        Chunk {
            header,
            main,
        },
    ))
}
