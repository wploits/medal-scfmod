use nom::bytes::complete::take;
use nom::number::complete::le_u8;
use nom::IResult;
use nom::error::ErrorKind;

use super::chunk::Chunk;

#[derive(Debug)]
pub enum Bytecode {
    Error(String),
    Chunk(Chunk),
}

impl Bytecode {
    pub fn parse(input: &[u8]) -> IResult<&[u8], Bytecode> {
        let (input, status_code) = le_u8(input)?;
        match status_code {
            0 => {
                let (input, error_msg) = take(input.len())(input)?;
                Ok((
                    input,
                    Bytecode::Error(String::from_utf8_lossy(error_msg).to_string()),
                ))
            },
            1 => panic!("Unsupported bytecode version"),
            _ => {
                let (input, chunk) = Chunk::parse(input)?;
                Ok((input, Bytecode::Chunk(chunk)))
            }
        }
    }
}
