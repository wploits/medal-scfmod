use nom::{
	Err,
	error::{Error, ErrorKind, ParseError},
	IResult,
	number::complete::le_u32,
};

#[derive(Debug)]
pub enum Layout {
	ABC { a: u8, b: u8, c: u8 },
	ABx { a: u8, bx: u32 },
	AsBx { a: u8, sbx: i32 },
}

impl Layout {
	pub fn parse(input: &[u8], operation_code: u8) -> IResult<&[u8], Self> {
		let (input, instruction) = le_u32(input)?;

		match operation_code {
			0 | 2..=4 | 6 | 8..=21 | 23..=30 | 33..=35 | 37 => {
				let a = ((instruction >> 6) & 0xFF) as u8;
				let c = ((instruction >> 14) & 0x1FF) as u8;
				let b = ((instruction >> 23) & 0x1FF) as u8;

				Ok((
					input,
					Self::ABC {
						a,
						b,
						c,
					},
				))
			}
			1 | 5 | 7 | 36 => {
				let a = ((instruction >> 6) & 0xFF) as u8;
				let bx = ((instruction >> 14) & 0x3FFFF) as u32;

				Ok((
					input,
					Self::ABx {
						a,
						bx,
					},
				))
			}
			22 | 31 | 32 => {
				let a = ((instruction >> 6) & 0xFF) as u8;
				let sbx = ((instruction >> 14) & 0x3FFFF) as i32;

				Ok((
					input,
					Self::AsBx {
						a,
						sbx,
					},
				))
			}
			_ => Err(Err::Failure(Error::from_error_kind(
				input,
				ErrorKind::Switch,
			))),
		}
	}
}