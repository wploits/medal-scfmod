use nom::{bytes::complete::take, number::complete::le_u8, IResult};

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
            }
            3 => {
                let (input, chunk) = Chunk::parse(input)?;
                Ok((input, Bytecode::Chunk(chunk)))
            }
            _ => panic!("Unsupported bytecode version"),
        }
    }
}
