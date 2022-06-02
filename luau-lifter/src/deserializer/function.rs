use nom::number::complete::{le_u32, le_u8};
use nom::IResult;
use nom_leb128::leb128_usize;

use super::constant::Constant;
use super::list::{parse_list, parse_list_len};

use crate::{
    instruction::*,
    op_code::OpCode,
};

#[derive(Debug)]
pub struct Function {
    pub max_stack_size: u8,
    pub num_parameters: u8,
    pub num_upvalues: u8,
    pub is_vararg: bool,
    //pub instructions: Vec<u32>,
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
    pub functions: Vec<usize>,
    pub line_defined: usize,
    pub function_name: usize,
    pub line_gap_log2: Option<u8>,
    pub line_info_delta: Option<Vec<u8>>,
    pub abs_line_info_delta: Option<Vec<u32>>,
}

impl Function {
    fn parse_instrution<'a>(input: &'a [u8]) -> IResult<&'a [u8], Instruction> {
        let (input, ins) = Instruction::parse(input)?;
        let op = match ins {
            Instruction::ABC { op_code, .. } => op_code,
            Instruction::AD { op_code, .. } => op_code,
            Instruction::E { op_code, .. } => op_code,
        };

        // handle ops with aux values
        match op {
            OpCode::LOP_GETGLOBAL |
            OpCode::LOP_SETGLOBAL |
            OpCode::LOP_GETIMPORT |
            OpCode::LOP_GETTABLEKS |
            OpCode::LOP_SETTABLEKS |
            OpCode::LOP_NAMECALL |
            OpCode::LOP_JUMPIFEQ |
            OpCode::LOP_JUMPIFLE |
            OpCode::LOP_JUMPIFLT |
            OpCode::LOP_JUMPIFNOTEQ |
            OpCode::LOP_JUMPIFNOTLE |
            OpCode::LOP_JUMPIFNOTLT |
            OpCode::LOP_NEWTABLE |
            OpCode::LOP_SETLIST |
            OpCode::LOP_FORGLOOP |
            OpCode::LOP_LOADKX |
            OpCode::LOP_JUMPIFEQK |
            OpCode::LOP_JUMPIFNOTEQK |
            OpCode::LOP_FASTCALL2 |
            OpCode::LOP_FASTCALL2K => {
                let (input, aux) = le_u32(input)?;
                match ins {
                    Instruction::ABC { op_code, a, b, c, .. } => {
                        Ok((input, Instruction::ABC {
                            op_code, a, b, c, aux
                        }))
                    }
                    Instruction::AD { op_code, a, d, .. } => {
                        Ok((input, Instruction::AD {
                            op_code, a, d, aux
                        }))
                    }
                    _ => unreachable!()
                }
            }
            _ => {
                Ok((input, ins))
            }
        }
    }

    pub(crate) fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, max_stack_size) = le_u8(input)?;
        let (input, num_parameters) = le_u8(input)?;
        let (input, num_upvalues) = le_u8(input)?;
        let (input, is_vararg) = le_u8(input)?;
        let (input, instructions) = parse_list(input, Function::parse_instrution)?;
        let (input, constants) = parse_list(input, Constant::parse)?;
        let (input, functions) = parse_list(input, leb128_usize)?;
        let (input, line_defined) = leb128_usize(input)?;
        let (input, function_name) = leb128_usize(input)?;
        let (input, has_line_info) = le_u8(input)?;
        let (input, line_gap_log2) = match has_line_info {
            0 => (input, None),
            _ => {
                let (input, line_gap_log2) = le_u8(input)?;
                (input, Some(line_gap_log2))
            }
        };
        let (input, line_info_delta) = match has_line_info {
            0 => (input, None),
            _ => {
                let (input, line_info_delta) = parse_list_len(input, le_u8, instructions.len())?;
                (input, Some(line_info_delta))
            }
        };
        let (input, abs_line_info_delta) = match has_line_info {
            0 => (input, None),
            _ => {
                let (input, abs_line_info_delta) = parse_list_len(input, le_u32, 
                    ((instructions.len() - 1) >> line_gap_log2.unwrap()) + 1)?;
                (input, Some(abs_line_info_delta))
            }
        };
        Ok((
            input,
            Self {
                max_stack_size,
                num_parameters,
                num_upvalues,
                is_vararg: is_vararg != 0u8,
                instructions,
                constants,
                functions,
                line_defined,
                function_name,
                line_gap_log2,
                line_info_delta,
                abs_line_info_delta
            },
        ))
    }
}
