use graph::NodeId;
use std::ops::Range;
use thiserror::Error;

use super::instruction::location::{InstructionIdx, InstructionLocation};

#[derive(Debug, Error)]
pub enum Error {
    #[error("Block {} does not exist", block)]
    InvalidBlock { block: NodeId },

    #[error("The specified instruction ({}) does not exist", instruction_location)]
    InvalidInstruction {
        instruction_location: InstructionLocation,
    },

    #[error("The specified instruction range ({}..{}) for block ({}) does not exist",
        instruction_location_range.start, instruction_location_range.end, block)]
    InvalidInstructionRange {
        block: NodeId,
        instruction_location_range: Range<InstructionIdx>,
    },

    #[error("No block has been selected")]
    NoBlockSelected,

    #[error("Index out of bounds")]
    IndexOutOfBounds,

    #[error("The block has no instructions")]
    NoInstructions,

    #[error("The block has no terminator")]
    NoTerminator,

    #[error("The block is sealed")]
    BlockSealed,

    #[error(transparent)]
    Graph(#[from] graph::error::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
