use nom::IResult;

pub use header::Header;

use crate::function::Function;

pub mod header;

#[derive(Debug)]
pub struct Chunk<'a> {
    pub header: Header,
    pub function: Function<'a>,
}

impl<'a> Chunk<'a> {
    pub fn parse(input: &'a [u8]) -> IResult<&[u8], Self> {
        let (input, header) = Header::parse(input)?;
        let (input, function) = Function::parse(input)?;

        Ok((input, Self { header, function }))
    }
}
