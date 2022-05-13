use super::NodeId;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Node {0} does not exist")]
    InvalidNode(NodeId),

    #[error("No entry node has been set")]
    NoEntry,
}
