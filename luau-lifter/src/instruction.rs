use std::convert::TryFrom;

use nom::{error::*, number::complete::le_u32, *};

use crate::op_code::OpCode;

/*

registers in () are not used
f prefix means no registers are used but its parsed as said type

LOP_NOP, f abc
LOP_BREAK, f abc
LOP_LOADNIL, a (bc)
LOP_LOADB, abc
LOP_LOADN, ad
LOP_LOADK, ad
LOP_MOVE, ab (c)
LOP_GETGLOBAL, a (b) c aux
LOP_SETGLOBAL, a (b) c aux
LOP_GETUPVAL, ab (c)
LOP_SETUPVAL, ab (c)
LOP_CLOSEUPVALS, a (bc)
LOP_GETIMPORT, ad aux
LOP_GETTABLE, abc
LOP_SETTABLE, abc
LOP_GETTABLEKS, abc aux
LOP_SETTABLEKS, abc aux
LOP_GETTABLEN, abc
LOP_SETTABLEN, abc
LOP_NEWCLOSURE, ad
LOP_NAMECALL, abc aux
LOP_CALL, abc
LOP_RETURN, ab (c)
LOP_JUMP, (a) d
LOP_JUMPBACK, (a) d
LOP_JUMPIF, ad
LOP_JUMPIFNOT, ad
LOP_JUMPIFEQ, ad aux
LOP_JUMPIFLE, ad aux
LOP_JUMPIFLT, ad aux
LOP_JUMPIFNOTEQ, ad aux
LOP_JUMPIFNOTLE, ad aux
LOP_JUMPIFNOTLT, ad aux
LOP_ADD, abc
LOP_SUB, abc
LOP_MUL, abc
LOP_DIV, abc
LOP_MOD, abc
LOP_POW, abc
LOP_ADDK, abc
LOP_SUBK, abc
LOP_MULK, abc
LOP_DIVK, abc
LOP_MODK, abc
LOP_POWK, abc
LOP_AND, abc
LOP_OR, abc
LOP_ANDK, abc
LOP_ORK, abc
LOP_CONCAT, abc
LOP_NOT, ab (c)
LOP_MINUS, ab (c)
LOP_LENGTH, ab (c)
LOP_NEWTABLE, ab (c) aux
LOP_DUPTABLE, ad
LOP_SETLIST, abc aux
LOP_FORNPREP, ad
LOP_FORNLOOP, ad
LOP_FORGLOOP, ad aux
LOP_FORGPREP_INEXT, ad
LOP_FORGLOOP_INEXT, ad
LOP_FORGPREP_NEXT, ad
LOP_FORGLOOP_NEXT, ad
LOP_GETVARARGS, ab (c)
LOP_DUPCLOSURE, ad
LOP_PREPVARARGS, a (bc)
LOP_LOADKX, a (bc) aux
LOP_JUMPX, e
LOP_FASTCALL, a (b) c
LOP_COVERAGE, e
LOP_CAPTURE, ab (c)
LOP_JUMPIFEQK, ad aux
LOP_JUMPIFNOTEQK, ad aux
LOP_FASTCALL1, abc
LOP_FASTCALL2, abc aux
LOP_FASTCALL2K, abc aux
LOP_FORGPREP, ad

*/

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
