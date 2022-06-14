use nom::{combinator::*, multi::*, number::complete::*, *};

use crate::{
    chunk::function::debug::{local::*, position::*, upvalue},
    instruction::*,
    value::*,
};

mod debug;

#[derive(Debug)]
pub struct Function<'a> {
    pub line_defined: u32,
    pub last_line_defined: u32,
    pub ellipsis_flag: u8,
    pub maximum_stack_size: u8,
    pub code: Vec<Instruction>,
    pub constants: Vec<ValueId<'a>>,
    pub closures: Vec<Function<'a>>,
    pub positions: Option<Vec<Position>>,
    pub locals: Option<Vec<Local<'a>>>,
    pub upvalues: Option<Vec<&'a str>>,
}

impl<'a> Function<'a> {
    pub fn parse(input: &'a [u8]) -> IResult<&'a [u8], Self> {
        let (input, _) = parse_str(input)?;
        let (input, line_defined) = le_u32(input)?;
        let (input, last_line_defined) = le_u32(input)?;
        let (input, _upvalues_length) = le_u8(input)?;
        let (input, _parameters_length) = le_u8(input)?;
        let (input, ellipsis_flag) = le_u8(input)?;
        let (input, maximum_stack_size) = le_u8(input)?;
        let (input, code_length) = le_u32(input)?;
        let (input, code) = count(Instruction::parse, code_length as usize)(input)?;
        let (input, constants_length) = le_u32(input)?;
        let (input, constants) = count(ValueId::parse, constants_length as usize)(input)?;
        let (input, closures_length) = le_u32(input)?;
        let (input, closures) = count(Self::parse, closures_length as usize)(input)?;
        let (input, positions) = opt(Position::parse)(input)?;
        let (input, locals) = opt(Local::parse)(input)?;
        let (input, upvalues) = opt(upvalue::parse)(input)?;

        Ok((
            input,
            Self {
                line_defined,
                last_line_defined,
                ellipsis_flag,
                maximum_stack_size,
                code,
                constants,
                closures,
                positions,
                locals,
                upvalues,
            },
        ))
    }
}
