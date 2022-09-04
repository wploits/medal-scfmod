use crate::{has_side_effects, Block, LValue, LocalRw, RValue, RcLocal, Traverse};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub initial: RValue,
    pub limit: RValue,
    pub step: RValue,
    pub counter: RcLocal,
    pub block: Block,
}

has_side_effects!(For);

impl For {
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

impl Traverse for For {
    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.initial, &mut self.limit, &mut self.step]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.initial, &self.limit, &self.step]
    }
}

impl LocalRw for For {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.initial
            .values_read()
            .into_iter()
            .chain(
                self.limit
                    .values_read()
                    .into_iter()
                    .chain(self.step.values_read()),
            )
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.initial
            .values_read_mut()
            .into_iter()
            .chain(
                self.limit
                    .values_read_mut()
                    .into_iter()
                    .chain(self.step.values_read_mut()),
            )
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        vec![&self.counter]
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        vec![&mut self.counter]
    }
}

impl fmt::Display for For {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
