pub mod edge;
pub mod error;
pub mod graph;

#[cfg(feature = "dot")]
pub mod dot;

pub type NodeId = usize;
pub type Result<T> = std::result::Result<T, error::Error>;

pub use graph::Graph;
