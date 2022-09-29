use fxhash::{FxHashMap, FxHashSet};
use petgraph::stable_graph::NodeIndex;

use crate::function::Function;

#[derive(Debug)]
pub(crate) struct UpvaluesOpen {
    open: FxHashMap<NodeIndex, FxHashSet<ast::RcLocal>>,
    old_locals: FxHashMap<ast::RcLocal, ast::RcLocal>,
}

impl UpvaluesOpen {
    pub fn new(function: &Function, old_locals: FxHashMap<ast::RcLocal, ast::RcLocal>) -> Self {
        let mut this = Self {
            open: Default::default(),
            old_locals,
        };
        let entry = function.entry().unwrap();
        let mut stack = vec![entry];
        let mut visited = FxHashSet::default();
        while let Some(node) = stack.pop() {
            visited.insert(node);
            let block = function.block(node).unwrap();
            let block_opened = this.open.entry(node).or_default();
            for statement in block.ast.iter() {
                if let ast::Statement::Assign(assign) = statement {
                    // TODO: collect_into when stabilized
                    block_opened.extend(
                        assign
                            .right
                            .iter()
                            .filter_map(|r| r.as_closure())
                            .flat_map(|c| c.upvalues.iter())
                            .cloned(),
                    );
                } else if let ast::Statement::Close(close) = statement {
                    let closed = close.locals.iter().collect::<FxHashSet<_>>();
                    block_opened.retain(|opened| !closed.contains(&&this.old_locals[opened]));
                }
            }
            for successor in function.successor_blocks(node) {
                if !visited.contains(&successor) {
                    let successor_opened =
                        this.open[&node].iter().cloned().collect::<FxHashSet<_>>();
                    this.open
                        .entry(successor)
                        .or_default()
                        .extend(successor_opened);
                    stack.push(successor);
                }
            }
        }
        this
    }

    pub fn is_open(
        &self,
        node: NodeIndex,
        index: usize,
        local: &ast::RcLocal,
        function: &Function,
    ) -> bool {
        let old_local = &self.old_locals[local];
        for statement in function
            .block(node)
            .unwrap()
            .ast
            .iter()
            .take(index + 1)
            .rev()
        {
            if let ast::Statement::Assign(assign) = statement {
                for opened in assign
                    .right
                    .iter()
                    .filter_map(|r| r.as_closure())
                    .flat_map(|c| c.upvalues.iter())
                {
                    if &self.old_locals[opened] == old_local {
                        return true;
                    }
                }
            } else if let ast::Statement::Close(close) = statement {
                for closed in &close.locals {
                    if closed == old_local {
                        return false;
                    }
                }
            }
        }

        for pred in function.predecessor_blocks(node) {
            if self.open[&pred]
                .iter()
                .any(|open_local| &self.old_locals[open_local] == old_local)
            {
                return true;
            }
        }

        false
    }
}
