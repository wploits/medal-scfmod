use crate::{has_side_effects, Block, LocalRw, RValue, RcLocal, Traverse};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct ForPrep {
    pub initial: RValue,
    pub limit: RValue,
    pub step: RValue,
    pub counter: RcLocal,
}

has_side_effects!(ForPrep);

impl ForPrep {
    pub fn new(
        initial: RValue,
        limit: RValue,
        step: RValue,
        counter: RcLocal,
        block: Block,
    ) -> Self {
        Self {
            initial,
            limit,
            step,
            counter,
        }
    }
}

impl LocalRw for ForPrep {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.initial
            .values_read()
            .into_iter()
            .chain(self.limit.values_read())
            .chain(self.step.values_read())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.initial
            .values_read_mut()
            .into_iter()
            .chain(self.limit.values_read_mut())
            .chain(self.step.values_read_mut())
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        vec![&self.counter]
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        vec![&mut self.counter]
    }
}

impl Traverse for ForPrep {
    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.initial, &self.limit, &self.step]
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.initial, &mut self.limit, &mut self.step]
    }
}

impl fmt::Display for ForPrep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "__for_prep({}, {}, {}, {})",
            self.initial, self.limit, self.step, self.counter
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForIterate {
    pub counter: RcLocal,
}

has_side_effects!(ForIterate);

impl ForIterate {
    pub fn new(counter: RcLocal) -> Self {
        Self { counter }
    }
}

impl Traverse for ForIterate {}

impl LocalRw for ForIterate {
    fn values_written(&self) -> Vec<&RcLocal> {
        vec![&self.counter]
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        vec![&mut self.counter]
    }
}

impl fmt::Display for ForIterate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "__for_iterate({})", self.counter)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumericFor {
    pub initial: RValue,
    pub limit: RValue,
    pub step: RValue,
    pub counter: RcLocal,
    pub block: Block,
}

has_side_effects!(NumericFor);

impl NumericFor {
    pub fn new(
        initial: RValue,
        limit: RValue,
        step: RValue,
        counter: RcLocal,
        block: Block,
    ) -> Self {
        Self {
            initial,
            limit,
            step,
            counter,
            block,
        }
    }
}

impl LocalRw for NumericFor {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.initial
            .values_read()
            .into_iter()
            .chain(self.limit.values_read())
            .chain(self.step.values_read())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.initial
            .values_read_mut()
            .into_iter()
            .chain(self.limit.values_read_mut())
            .chain(self.step.values_read_mut())
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        vec![&self.counter]
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        vec![&mut self.counter]
    }
}

impl Traverse for NumericFor {
    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.initial, &self.limit, &self.step]
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.initial, &mut self.limit, &mut self.step]
    }
}

impl fmt::Display for NumericFor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "for {} = {}, {}, {} do\n{}\nend",
            self.counter,
            self.initial,
            self.limit,
            self.step,
            self.block
                .iter()
                .map(|n| n.to_string().replace('\n', "\n\t"))
                .join("\n\t")
        )
    }
}
