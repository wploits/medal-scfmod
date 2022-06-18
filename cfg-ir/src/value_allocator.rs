use crate::value::ValueId;

#[derive(Debug)]
pub struct ValueAllocator {
    pub(crate) next_value_index: usize,
}

impl ValueAllocator {
    pub fn new() -> Self {
        Self {
            next_value_index: 0,
        }
    }

    pub fn new_value(&mut self) -> ValueId {
        let value = self.next_value_index;
        self.next_value_index += 1;
        ValueId(value)
    }

    pub fn values(&self) -> Vec<ValueId> {
        (0..self.next_value_index).map(ValueId).collect()
    }
}

impl Default for ValueAllocator {
    fn default() -> Self {
        Self::new()
    }
}