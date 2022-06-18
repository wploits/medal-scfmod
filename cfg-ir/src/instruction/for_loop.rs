use graph::NodeId;
use std::fmt;

use super::{super::value::ValueId, branch_info::BranchInfo, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct NumericFor {
    pub variable: ValueId,
    pub init: ValueId,
    pub limit: ValueId,
    pub step: ValueId,
    pub continue_branch: NodeId,
    pub exit_branch: NodeId,
}

impl ValueInfo for NumericFor {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.limit, self.init, self.step, self.variable]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![self.variable]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.limit, &mut self.init, &mut self.step, &mut self.variable]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.variable]
    }
}

impl BranchInfo for NumericFor {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.continue_branch, self.exit_branch]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.continue_branch, &mut self.exit_branch]
    }
}

impl fmt::Display for NumericFor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} <- {} + {}, continue {{{}}} or break {{{}}}",
            self.variable, self.variable, self.step, self.continue_branch, self.exit_branch
        )
    }
}
