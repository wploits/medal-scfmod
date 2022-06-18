use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use graph::NodeId;
use std::fmt;

use super::{super::value::ValueId, branch_info::BranchInfo, value_info::ValueInfo};

use super::for_loop::{NumericFor};

#[derive(Debug, Clone)]
pub struct Return {
    pub values: Vec<ValueId>,
    pub variadic: bool,
}

impl ValueInfo for Return {
    fn values_read(&self) -> Vec<ValueId> {
        self.values.clone()
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        self.values.iter_mut().collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl BranchInfo for Return {
    fn branches(&self) -> Vec<NodeId> {
        Vec::new()
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        Vec::new()
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "return {}",
            self.values
                .iter()
                .map(|v| v.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )?;
        if self.variadic {
            if !self.values.is_empty() {
                write!(f, ", ")?;
            }
            write!(f, "...")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ConditionalJump {
    pub condition: ValueId,
    pub true_branch: NodeId,
    pub false_branch: NodeId,
}

impl ValueInfo for ConditionalJump {
    fn values_read(&self) -> Vec<ValueId> {
        vec![self.condition]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![&mut self.condition]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl BranchInfo for ConditionalJump {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.true_branch, self.false_branch]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.true_branch, &mut self.false_branch]
    }
}

impl fmt::Display for ConditionalJump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if {} {{{}}} else {{{}}}",
            self.condition, self.true_branch, self.false_branch
        )
    }
}

#[derive(Debug, Clone)]
pub struct UnconditionalJump(pub NodeId);

impl ValueInfo for UnconditionalJump {
    fn values_read(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_written(&self) -> Vec<ValueId> {
        vec![]
    }

    fn values_read_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }

    fn values_written_mut(&mut self) -> Vec<&mut ValueId> {
        vec![]
    }
}

impl BranchInfo for UnconditionalJump {
    fn branches(&self) -> Vec<NodeId> {
        vec![self.0]
    }

    fn branches_mut(&mut self) -> Vec<&mut NodeId> {
        vec![&mut self.0]
    }
}

impl fmt::Display for UnconditionalJump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "jmp")
    }
}

#[enum_dispatch(BranchInfo, ValueInfo)]
#[derive(Debug, Clone, EnumAsInner)]
pub enum Terminator {
    Return(Return),
    ConditionalJump(ConditionalJump),
    UnconditionalJump(UnconditionalJump),
    NumericFor(NumericFor),
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(x) => x.fmt(f),
            Terminator::ConditionalJump(x) => x.fmt(f),
            Terminator::UnconditionalJump(x) => x.fmt(f),
            Terminator::NumericFor(x) => x.fmt(f),
        }
    }
}
