use super::NodeId;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Node {} does not exist", node)]
    InvalidNode { node: NodeId },

    #[error("No entry node has been set")]
    NoEntry,
}
