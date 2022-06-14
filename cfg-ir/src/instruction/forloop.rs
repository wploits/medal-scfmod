use std::fmt;
use graph::NodeId;

use super::{super::value::ValueId, branch_info::BranchInfo, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct NumericFor {
    pub limit: ValueId,
    pub step: ValueId,
    pub variable: ValueId, // luau has an internal index variable but its never exposed to lua code so maybe this will suffice?
    pub true_branch: NodeId,
    pub false_branch: NodeId,
}

impl ValueInfo for NumericFor {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.limit, self.step, self.variable]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.limit, &mut self.step, &mut self.variable]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl BranchInfo for NumericFor {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.true_branch, self.false_branch]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.true_branch, &mut self.false_branch]
    }
}

impl fmt::Display for NumericFor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "for {} {} {}",
            self.limit, self.step, self.variable
        )
    }
}
