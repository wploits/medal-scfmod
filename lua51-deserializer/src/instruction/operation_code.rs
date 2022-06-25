use nom::{
	Err,
	error::{Error, ErrorKind, ParseError},
	IResult,
	number::complete::le_u8,
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
	GetTable,
	SetGlobal,
	SetUpvalue,
	SetTable,
	NewTable,
	Self_,
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Power,
	Minus,
	Negate,
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
	PrepareNumericForLoop,
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

		Ok((input,
			match FromPrimitive::from_u8(operation_code) {
				None => return Err(Err::Failure(
						Error::from_error_kind(input, ErrorKind::Switch)
					)
				),
				Some(operation_code) => operation_code,
			},
		))
	}
}