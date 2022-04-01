use super::NodeId;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Edge(pub NodeId, pub NodeId);

impl Edge {
    pub fn source(&self) -> NodeId {
        self.0
    }

    pub fn destination(&self) -> NodeId {
        self.1
    }
}
