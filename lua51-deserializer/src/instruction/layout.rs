use nom::{
    error::{Error, ErrorKind, ParseError},
    number::complete::le_u32,
    Err, IResult,
};
use num_traits::FromPrimitive;

use super::OperationCode;

#[derive(Debug)]
pub enum Layout {
    ABC { a: u8, b: u16, c: u16 },
    ABx { a: u8, bx: u32 },
    AsBx { a: u8, sbx: i32 },
}

impl Layout {
    pub fn parse(input: &[u8], operation_code: u8) -> IResult<&[u8], Self> {
        let (input, instruction) = le_u32(input)?;

        match FromPrimitive::from_u8(operation_code).map(|o: OperationCode| o.instruction_layout())
        {
            Some(0) => {
                let a = ((instruction >> 6) & 0xFF) as u8;
                let c = ((instruction >> 14) & 0x1FF) as u16;
                let b = ((instruction >> 23) & 0x1FF) as u16;

                Ok((input, Self::ABC { a, b, c }))
            }
            Some(1) => {
                let a = ((instruction >> 6) & 0xFF) as u8;
                let bx = ((instruction >> 14) & 0x3FFFF) as u32;

                Ok((input, Self::ABx { a, bx }))
            }
            Some(2) => {
                let a = ((instruction >> 6) & 0xFF) as u8;
                let sbx = ((instruction >> 14) & 0x3FFFF) as i32;

                Ok((input, Self::AsBx { a, sbx }))
            }
            _ => Err(Err::Failure(Error::from_error_kind(
                input,
                ErrorKind::Switch,
            ))),
        }
    }
}
