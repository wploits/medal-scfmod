use std::convert::TryFrom;

use nom::{*, error::*, number::complete::le_u32};

use crate::op_code::OpCode;

#[derive(Debug)]
pub enum Instruction {
    ABC {
        op_code: OpCode,
        a: u8,
        b: u16,
        c: u16,
    },
    ABx {
        op_code: OpCode,
        a: u8,
        bx: u32,
    },
    AsBx {
        op_code: OpCode,
        a: u8,
        sbx: i32,
    },
}

impl Instruction {
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, insn) = le_u32(input)?;
        let op_code = (insn & 0x3F) as u8;

        match op_code {
            0 | 2..=4 | 6 | 8..=21 | 23..=30 | 33..=35 | 37 => {
                let (a, b, c) = Self::parse_abc(insn);

                Ok((
                    input,
                    Self::ABC {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        a,
                        b,
                        c,
                    },
                ))
            }
            1 | 5 | 7 | 36 => {
                let (a, bx) = Self::parse_abx(insn);

                Ok((
                    input,
                    Self::ABx {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        a,
                        bx,
                    },
                ))
            }
            22 | 31 | 32 => {
                let (a, bx) = Self::parse_abx(insn);

                Ok((
                    input,
                    Self::AsBx {
                        op_code: OpCode::try_from(op_code).unwrap(),
                        a,
                        sbx: bx as i32,
                    },
                ))
            }
            _ => Err(Err::Failure(Error::from_error_kind(
                input,
                ErrorKind::Switch,
            ))),
        }
    }

    pub fn opcode(&self) -> OpCode {
        *match self {
            Instruction::ABC { op_code, .. } => op_code,
            Instruction::ABx { op_code, .. } => op_code,
            Instruction::AsBx { op_code, .. } => op_code,
        }
    }

    fn parse_abc(insn: u32) -> (u8, u16, u16) {
        let a = ((insn >> 6) & 0xFF) as u8;
        let c = ((insn >> 14) & 0x1FF) as u16;
        let b = ((insn >> 23) & 0x1FF) as u16;

        (a, b, c)
    }

    fn parse_abx(insn: u32) -> (u8, u32) {
        let a = ((insn >> 6) & 0xFF) as u8;
        let bx = ((insn >> 14) & 0x3FFFF) as u32;

        (a, bx)
    }
}
