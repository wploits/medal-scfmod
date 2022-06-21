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
        vec![
            &mut self.limit,
            &mut self.init,
            &mut self.step,
            &mut self.variable,
        ]
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
            "for {} {} {} {} {{{}}} or break {{{}}}",
            self.variable, self.init, self.step, self.limit, self.continue_branch, self.exit_branch
        )
    }
}

#[derive(Debug, Clone)]
pub struct IterativeFor {
    pub generator: ValueId,
    pub state: ValueId,
    pub index: ValueId,
    pub variables: Vec<ValueId>,
    pub continue_branch: NodeId,
    pub exit_branch: NodeId,
}

impl BranchInfo for IterativeFor {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.continue_branch, self.exit_branch]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.continue_branch, &mut self.exit_branch]
    }
}

impl ValueInfo for IterativeFor {
    fn values_read(&self) -> Vec<ValueId> {
        let mut v = vec![self.generator, self.state, self.index];
        v.extend_from_slice(&self.variables);
        v
    }

    fn values_written(&self) -> Vec<ValueId> {
        self.variables.clone()
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        let mut v = vec![&mut self.generator, &mut self.state, &mut self.index];
        v.extend(self.variables.iter_mut());
        v
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        self.variables.iter_mut().collect()
    }
}

impl fmt::Display for IterativeFor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "for {{{} vars}} in {} {} {} {{{}}} or break {{{}}}",
            self.variables.len(),
            self.generator,
            self.state,
            self.index,
            self.continue_branch,
            self.exit_branch
        )
    }
}
