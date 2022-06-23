use nom::{
	Err,
	error::{Error, ErrorKind, ParseError},
	IResult,
	number::complete::{le_f64, le_u8, le_u32},
	bytes::complete::take,
	multi::count,
};

#[derive(Debug)]
pub enum Value<'a> {
	Nil,
	Boolean(bool),
	Number(f64),
	String(&'a str),
}

impl<'a> Value<'a> {
	pub fn parse(input: &'a [u8]) -> IResult<&'a [u8], Self> {
		let (input, kind) = le_u8(input)?;

		match kind {
			0 => Ok((input, Self::Nil)),
			1 => {
				let (input, value) = le_u8(input)?;

				Ok((input, Self::Boolean(value != 0)))
			}
			3 => {
				let (input, value) = le_f64(input)?;

				Ok((input, Self::Number(value)))
			}
			4 => {
				let (input, value) = parse_string(input)?;

				Ok((input, Self::String(&value[..value.len() - 1])))
			}
			_ => Err(Err::Failure(Error::from_error_kind(
				input,
				ErrorKind::Switch,
			))),
		}
	}
}

pub fn parse_string(input: &[u8]) -> IResult<&[u8], &str> {
	let (input, name_length) = le_u32(input)?;
	let (input, string) = take(name_length as usize)(input)?;

	Ok((input, unsafe { std::str::from_utf8_unchecked(string) }))
}

pub fn parse_strings(input: &[u8]) -> IResult<&[u8], Vec<&str>> {
	let (input, upvalues_length) = le_u32(input)?;
	let (input, upvalues) = count(parse_string, upvalues_length as usize)(input)?;

	Ok((input, upvalues))
}

