use graph::NodeId;
use std::ops::Range;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Block {0} does not exist")]
    InvalidBlock(NodeId),

    /*#[error("The specified instruction {0} does not exist")]
    InvalidInstruction(InstructionLocation),

    #[error("The specified instruction range {}..{} for block {} does not exist",
        instruction_location_range.start, instruction_location_range.end, block)]
    InvalidInstructionRange {
        block: NodeId,
        instruction_location_range: Range<InstructionIndex>,
    },*/

    #[error("No block has been selected")]
    NoBlockSelected,

    #[error("Index {0} out of bounds")]
    IndexOutOfBounds(usize),

    #[error("The block has no instructions")]
    NoInstructions,

    #[error("The block has no terminator")]
    NoTerminator,

    #[error("The block is sealed")]
    BlockSealed,

    #[error(transparent)]
    Graph(#[from] graph::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
