use super::stat::Block;

#[derive(Debug, Default)]
pub struct Function {
    pub name: Option<String>,
    pub body: Block,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: None,
            body: Block::new(None),
        }
    }
}
