use nom::{
    error::{Error, ErrorKind, ParseError},
    number::complete::le_u8,
    Err, IResult,
};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;

#[derive(Debug, FromPrimitive, ToPrimitive)]
pub enum OperationCode {
    Move = 0,
    LoadConstant,
    LoadBoolean,
    LoadNil,
    GetUpvalue,
    GetGlobal,
    GetIndex,
    SetGlobal,
    SetUpvalue,
    SetIndex,
    NewTable,
    PrepMethodCall,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Minus,
    Not,
    Length,
    Concatenate,
    Jump,
    Equal,
    LessThan,
    LessThanOrEqual,
    Test,
    TestSet,
    Call,
    TailCall,
    Return,
    IterateNumericForLoop,
    InitNumericForLoop,
    IterateGenericForLoop,
    SetList,
    Close,
    Closure,
    VarArg,
}

impl OperationCode {
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, operation_code) = le_u8(input)?;
        let operation_code = operation_code & 0x3F;

        Ok((
            input,
            match FromPrimitive::from_u8(operation_code) {
                None => {
                    return Err(Err::Failure(Error::from_error_kind(
                        input,
                        ErrorKind::Switch,
                    )))
                }
                Some(operation_code) => operation_code,
            },
        ))
    }

    pub fn instruction_layout(&self) -> usize {
        /*
           0 = ABC
           1 = ABx
           2 = AsBx
        */

        match self {
            Self::Move => 0,
            Self::LoadConstant => 1,
            Self::LoadBoolean => 0,
            Self::LoadNil => 0,
            Self::GetUpvalue => 0,
            Self::GetGlobal => 1,
            Self::GetIndex => 0,
            Self::SetGlobal => 1,
            Self::SetUpvalue => 0,
            Self::SetIndex => 0,
            Self::NewTable => 0,
            Self::PrepMethodCall => 0,
            Self::Add => 0,
            Self::Subtract => 0,
            Self::Multiply => 0,
            Self::Divide => 0,
            Self::Modulo => 0,
            Self::Power => 0,
            Self::Minus => 0,
            Self::Not => 0,
            Self::Length => 0,
            Self::Concatenate => 0,
            Self::Jump => 2,
            Self::Equal => 0,
            Self::LessThan => 0,
            Self::LessThanOrEqual => 0,
            Self::Test => 0,
            Self::TestSet => 0,
            Self::Call => 0,
            Self::TailCall => 0,
            Self::Return => 0,
            Self::IterateNumericForLoop => 2,
            Self::InitNumericForLoop => 2,
            Self::IterateGenericForLoop => 0,
            Self::SetList => 0,
            Self::Close => 0,
            Self::Closure => 1,
            Self::VarArg => 0,
        }
    }
}
