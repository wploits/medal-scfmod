use super::stat::Block;

#[derive(Debug, Default)]
pub struct Function<'ast> {
    pub name: Option<String>,
    pub body: Block<'ast>,
}

impl<'ast> Function<'ast> {
    pub fn new() -> Self {
        Self {
            name: None,
            body: Block::new(None),
        }
    }
}
