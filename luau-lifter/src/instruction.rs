use std::convert::TryFrom;

use nom::{error::*, number::complete::le_u32, *};

use crate::op_code::OpCode;

#[derive(Debug)]
pub enum Instruction {
    ABC {
        op_code: OpCode,
        a: u8,
        b: u8,
        c: u8,
        aux: u32
    },
    AD {
        op_code: OpCode,
        a: u8,
        d: i16,
        aux: u32
    },
    E {
        op_code: OpCode,
        e: i32,
    },
}

impl Instruction {
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, insn) = le_u32(input)?;
        let op_code = (insn & 0xFF) as u8;

        match op_code {
            0 | 1 | 3 | 13..=18 | 20 | 21 | 33..=49 | 55 | 73..=75 => {
                let (a, b, c) = Self::parse_abc(insn);

                Ok((
                    input,
                    Self::ABC {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        a,
                        b,
                        c,
                        aux: 0
                    },
                ))
            }
            4 | 5 | 12 | 19 | 25..=32 | 54 | 56..=62 | 64 | 71 | 72 | 76 => {
                let (a, d) = Self::parse_ad(insn);

                Ok((
                    input,
                    Self::AD {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        a,
                        d,
                        aux: 0
                    },
                ))
            }
            67 | 69 => {
                let e = Self::parse_e(insn);

                Ok((
                    input,
                    Self::E {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        e
                    },
                ))
            }
            _ => Err(Err::Failure(Error::from_error_kind(
                input,
                ErrorKind::Switch,
            ))),
        }
    }

    fn parse_abc(insn: u32) -> (u8, u8, u8) {
        let a = ((insn >> 8) & 0xFF) as u8;
        let c = ((insn >> 16) & 0xFF) as u8;
        let b = ((insn >> 24) & 0xFF) as u8;

        (a, b, c)
    }

    fn parse_ad(insn: u32) -> (u8, i16) {
        let a = ((insn >> 8) & 0xFF) as u8;
        let d = ((insn >> 16) & 0xFFFF) as i16;

        (a, d)
    }

    fn parse_e(insn: u32) -> i32 {
        let d = ((insn >> 8) & 0xFFFFFF) as i32;

        d
    }
}
