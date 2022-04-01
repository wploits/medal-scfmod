use enum_dispatch::enum_dispatch;

use crate::value::ValueId;

// A trait implemented for instructions that read or write a value
#[enum_dispatch]
pub(crate) trait ValueInfo {
    fn values_read(&self) -> Box<[ValueId]>;
    fn values_read_mut(&mut self) -> Box<[&mut ValueId]>;
    fn values_written(&self) -> Box<[ValueId]>;
    fn values_written_mut(&mut self) -> Box<[&mut ValueId]>;

    fn values(&self) -> Box<[ValueId]> {
        let mut res = Vec::from(self.values_read());
        res.extend(self.values_written().iter());
        res.into_boxed_slice()
    }

    fn replace_values_read(&mut self, old: ValueId, new: ValueId) {
        for value in self.values_read_mut().iter_mut() {
            if **value == old {
                **value = new;
            }
        }
    }

    fn replace_values_written(&mut self, old: ValueId, new: ValueId) {
        for value in self.values_written_mut().iter_mut() {
            if **value == old {
                **value = new;
            }
        }
    }

    fn replace_values(&mut self, old: ValueId, new: ValueId) {
        self.replace_values_read(old, new);
        self.replace_values_written(old, new);
    }
}
