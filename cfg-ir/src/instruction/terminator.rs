use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use graph::NodeId;
use std::fmt;

use super::{super::value::ValueId, branch_info::BranchInfo, value_info::ValueInfo};

#[derive(Debug, Clone)]
pub struct Return {
    pub values: Vec<ValueId>,
    pub variadic: bool,
}

impl ValueInfo for Return {
    fn values_read(&self) -> Box<[ValueId]> {
        self.values.clone().into_boxed_slice()
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        self.values.iter_mut().collect()
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }
}

impl BranchInfo for Return {
    fn branches(&self) -> Box<[NodeId]> {
        Box::new([])
    }

    fn branches_mut(&mut self) -> Box<[&mut NodeId]> {
        Box::new([])
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
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([self.condition])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([&mut self.condition])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }
}

impl BranchInfo for ConditionalJump {
    fn branches(&self) -> Box<[NodeId]> {
        Box::new([self.true_branch, self.false_branch])
    }

    fn branches_mut(&mut self) -> Box<[&mut NodeId]> {
        Box::new([&mut self.true_branch, &mut self.false_branch])
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
    fn values_read(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_written(&self) -> Box<[ValueId]> {
        Box::new([])
    }

    fn values_read_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }

    fn values_written_mut(&mut self) -> Box<[&mut ValueId]> {
        Box::new([])
    }
}

impl BranchInfo for UnconditionalJump {
    fn branches(&self) -> Box<[NodeId]> {
        Box::new([self.0])
    }

    fn branches_mut(&mut self) -> Box<[&mut NodeId]> {
        Box::new([&mut self.0])
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
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(x) => x.fmt(f),
            Terminator::ConditionalJump(x) => x.fmt(f),
            Terminator::UnconditionalJump(x) => x.fmt(f),
        }
    }
}
