use enum_dispatch::enum_dispatch;

use crate::value::ValueId;

// A trait implemented for instructions that read or write a value
#[enum_dispatch]
pub trait ValueInfo {
    fn values_read(&self) -> Vec<ValueId>;
    fn values_read_mut(&mut self) -> Vec<&mut ValueId>;
    fn values_written(&self) -> Vec<ValueId>;
    fn values_written_mut(&mut self) -> Vec<&mut ValueId>;

    fn values(&self) -> Vec<ValueId> {
        let mut res = Vec::from(self.values_read());
        res.extend(self.values_written().iter());
        res
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
