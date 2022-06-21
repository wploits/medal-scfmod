pub struct Type {
    name: String,
    index: HashMap<String, Type>,
}

impl Type {
    pub fn index(&self, key: String) -> Option<Type> {
        self.index.get(key)
    }
}

pub trait NodeType {
    fn node_type(&self) -> Type;
}
