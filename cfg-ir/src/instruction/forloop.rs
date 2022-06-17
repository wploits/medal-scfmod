use graph::NodeId;
use std::fmt;

use super::{super::value::ValueId, branch_info::BranchInfo, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct NumericForEnter {
    pub variable: ValueId,
    pub init: ValueId,
    pub node: NodeId,
}

impl ValueInfo for NumericForEnter {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.init]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.variable]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.init]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.variable]
    }
}

impl BranchInfo for NumericForEnter {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.node]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.node]
    }
}

impl fmt::Display for NumericForEnter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {}, enter for loop", self.variable, self.init)
    }
}


#[derive(Debug, Clone)]
pub struct NumericForLoop {
    pub variable: ValueId,
    pub limit: ValueId,
    pub step: ValueId,
    pub true_branch: NodeId,
    pub false_branch: NodeId,
}

impl ValueInfo for NumericForLoop {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.limit, self.step, self.variable]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.variable]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.limit, &mut self.step, &mut self.variable]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.variable]
    }
}

impl BranchInfo for NumericForLoop {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.true_branch, self.false_branch]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.true_branch, &mut self.false_branch]
    }
}

impl fmt::Display for NumericForLoop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} <- {} + {}, if equal to {} {{{}}} else continue for loop {{{}}}", self.variable, self.variable, self.step, self.limit, self.false_branch, self.true_branch)
    }
}
