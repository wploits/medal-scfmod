use crate::{has_side_effects, Block, LValue, LocalRw, RValue, RcLocal, Traverse};
use itertools::Itertools;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct NumForInit {
    // TODO: store 3 `Assign`s instead
    pub counter: (LValue, RValue),
    pub limit: (LValue, RValue),
    pub step: (LValue, RValue),
}

impl NumForInit {
    pub fn from_locals(counter: RcLocal, limit: RcLocal, step: RcLocal) -> Self {
        Self {
            counter: (LValue::Local(counter.clone()), RValue::Local(counter)),
            limit: (LValue::Local(limit.clone()), RValue::Local(limit)),
            step: (LValue::Local(step.clone()), RValue::Local(step)),
        }
    }
}

// TODO: true if any LValues/RValues have side effects
// same problem exists in other places, just search for "has_side_effects!"
has_side_effects!(NumForInit);

// TODO: this treats this as
//      a, b, c = 1, 2, 3
// rather than
//      a = 1; b = 2; b = 3
// which is what is actually happening here
// but it *should* be fine
// but there are some situations in which some constructed bytecode could
// get the better of us
impl Traverse for NumForInit {
    fn lvalues_mut(&mut self) -> Vec<&mut LValue> {
        vec![&mut self.counter.0, &mut self.limit.0, &mut self.step.0]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.counter.1, &self.limit.1, &self.step.1]
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.counter.1, &mut self.limit.1, &mut self.step.1]
    }
}

// TODO: this treats this as
//      a, b, c = 1, 2, 3
// rather than
//      a = 1; b = 2; b = 3
// which is what is actually happening here
// but it *should* be fine
// but there are some situations in which some constructed bytecode could
// get the better of us
impl LocalRw for NumForInit {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.counter
            .1
            .values_read()
            .into_iter()
            .chain(self.limit.1.values_read())
            .chain(self.step.1.values_read().into_iter())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.counter
            .1
            .values_read_mut()
            .into_iter()
            .chain(self.limit.1.values_read_mut())
            .chain(self.step.1.values_read_mut().into_iter())
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        self.counter
            .0
            .values_written()
            .into_iter()
            .chain(self.limit.0.values_written())
            .chain(self.step.0.values_written().into_iter())
            .collect()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        self.counter
            .0
            .values_written_mut()
            .into_iter()
            .chain(self.limit.0.values_written_mut())
            .chain(self.step.0.values_written_mut().into_iter())
            .collect()
    }
}

impl fmt::Display for NumForInit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "-- NumForInit\n{} = {}\n{} = {}\n{} = {}\n-- end NumForInit",
            self.counter.0, self.counter.1, self.limit.0, self.limit.1, self.step.0, self.step.1
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumForNext {
    // TODO: store an `Assign` and an `If` instead?
    // TODO: this is the worst s$H##()WT ever literally
    pub counter: (LValue, RValue), // RcLocal, // cant be of type RcLocal because Traverse
    pub limit: RValue,
    pub step: RValue,
}

// TODO: true if any LValues/RValues have side effects
// same problem exists in other places, just search for "has_side_effects!"
has_side_effects!(NumForNext);

impl NumForNext {
    pub fn new(counter: RcLocal, limit: RValue, step: RValue) -> Self {
        Self {
            counter: (LValue::Local(counter.clone()), RValue::Local(counter)),
            limit,
            step,
        }
    }
}

impl Traverse for NumForNext {
    fn lvalues_mut(&mut self) -> Vec<&mut LValue> {
        vec![&mut self.counter.0]
    }

    fn rvalues(&self) -> Vec<&RValue> {
        vec![&self.counter.1, &self.step, &self.limit]
    }

    fn rvalues_mut(&mut self) -> Vec<&mut RValue> {
        vec![&mut self.counter.1, &mut self.step, &mut self.limit]
    }
}

impl LocalRw for NumForNext {
    fn values_read(&self) -> Vec<&RcLocal> {
        self.counter
            .1
            .values_read()
            .into_iter()
            .chain(self.step.values_read().into_iter())
            .chain(self.limit.values_read())
            .collect()
    }

    fn values_read_mut(&mut self) -> Vec<&mut RcLocal> {
        self.counter
            .1
            .values_read_mut()
            .into_iter()
            .chain(self.step.values_read_mut().into_iter())
            .chain(self.limit.values_read_mut())
            .collect()
    }

    fn values_written(&self) -> Vec<&RcLocal> {
        self.counter.0.values_written()
    }

    fn values_written_mut(&mut self) -> Vec<&mut RcLocal> {
        self.counter.0.values_written_mut()
    }
}

impl fmt::Display for NumForNext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "-- NumForNext\n{} = {} + {};\nif {} <= {}\n-- end NumForNext",
            self.counter.0, self.counter.1, self.step, self.counter.0, self.limit
        )
    }
}

// TODO: this should probably be named "NumFor"
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
