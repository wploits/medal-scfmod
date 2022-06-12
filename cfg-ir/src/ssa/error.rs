use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Graph(#[from] graph::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
