use either::Either;

#[derive(Debug)]
pub struct Register(pub u8);

#[derive(Debug)]
pub struct Constant(pub u32);

#[derive(Debug)]
pub struct RegisterOrConstant(Either<Register, Constant>);

impl From<u32> for RegisterOrConstant {
	fn from(value: u32) -> Self {
		Self(
			if value > 255 {
				Either::Right(Constant(value - 255))
			} else {
				Either::Left(Register(value as u8))
			}
		)
	}
}


#[derive(Debug)]
pub struct Upvalue(pub u8);

#[derive(Debug)]
pub struct Function(pub u32);
