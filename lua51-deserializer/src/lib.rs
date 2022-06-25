pub use function::Function;
pub use instruction::{argument, Instruction};
pub use value::Value;

pub mod instruction;
pub mod value;
pub mod local;
pub mod chunk;
pub mod function;

