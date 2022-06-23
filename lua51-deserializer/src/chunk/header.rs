use nom::{
	bytes::complete::tag,
	Err,
	error::{Error, ErrorKind, ParseError},
	IResult,
	number::complete::le_u8,
};

#[derive(Debug)]
pub enum Endianness {
	Big,
	Little,
}

#[derive(Debug)]
pub enum IntegralFlag {
	FloatingPoint,
	Integral,
}

#[derive(Debug)]
pub enum Version {
	Official,
}

#[derive(Debug)]
pub struct Header {
	version_number: u8,
	version: Version,
	endianness: Endianness,
	size_of_integer: u8,
	size_of_size_t: u8,
	size_of_instruction: u8,
	size_of_lua_number: u8,
	integral_flag: IntegralFlag,
}

impl Header {
	pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
		let (input, _) = tag("\x1BLua")(input)?;
		let (input, version_number) = le_u8(input)?;
		let (input, version) = match le_u8(input)? {
			(input, 0) => Ok((input, Version::Official)),
			_ => Err(Err::Failure(Error::from_error_kind(
				input,
				ErrorKind::Switch,
			))),
		}?;
		let (input, endianness) = match le_u8(input)? {
			(input, 0) => Ok((input, Endianness::Big)),
			(input, 1) => Ok((input, Endianness::Little)),
			_ => Err(Err::Failure(Error::from_error_kind(
				input,
				ErrorKind::Switch,
			))),
		}?;
		let (input, size_of_integer) = le_u8(input)?;
		let (input, size_of_size_t) = le_u8(input)?;
		let (input, size_of_instruction) = le_u8(input)?;
		let (input, size_of_lua_number) = le_u8(input)?;
		let (input, integral_flag) = match le_u8(input)? {
			(input, 0) => Ok((input, IntegralFlag::FloatingPoint)),
			(input, 1) => Ok((input, IntegralFlag::Integral)),
			_ => Err(Err::Failure(Error::from_error_kind(
				input,
				ErrorKind::Switch,
			))),
		}?;

		Ok((
			input,
			Self {
				version_number,
				version,
				endianness,
				size_of_integer,
				size_of_size_t,
				size_of_instruction,
				size_of_lua_number,
				integral_flag,
			},
		))
	}
}