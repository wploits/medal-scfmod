use ast::Traverse;
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::Dfs};

use crate::function::Function;

pub(crate) fn statement_upvalues_opened(statement: &ast::Statement) -> Vec<&ast::RcLocal> {
    let mut upvalues_opened = Vec::new();
    for rvalue in statement.rvalues() {
        if let ast::RValue::Closure(closure) = rvalue {
            upvalues_opened.extend(closure.upvalues.iter());
        }
    }
    upvalues_opened
}

pub(crate) fn statement_upvalues_closed(statement: &ast::Statement) -> Vec<&ast::RcLocal> {
    if let ast::Statement::Close(close) = statement {
        close.locals.iter().collect()
    } else {
        Vec::new()
    }
}

#[derive(Debug)]
pub(crate) struct UpvaluesOpen(FxHashMap<NodeIndex, FxHashSet<(ast::RcLocal, (NodeIndex, usize))>>);

impl UpvaluesOpen {
    pub fn new(function: &Function) -> Self {
        let mut this = Self(Default::default());
        let entry = function.entry().unwrap();
        let mut stack = vec![entry];
        let mut visited = FxHashSet::default();
        while let Some(node) = stack.pop() {
            visited.insert(node);
            let block = function.block(node).unwrap();
            let mut block_opened = FxHashSet::default();
            for (stat_index, statement) in block.ast.iter().enumerate() {
                let statement_opened = statement_upvalues_opened(statement);
                if !statement_opened.is_empty() {
                    block_opened.extend(
                        statement_opened
                            .into_iter()
                            .cloned()
                            .map(|opened| (opened, (node, stat_index))),
                    );
                } else {
                    let statement_closed = statement_upvalues_closed(statement);
                    block_opened.retain(|(opened, _)| !statement_closed.contains(&opened));
                }
            }
            this.0
                .entry(node)
                .or_default()
                .extend(block_opened.iter().cloned());
            for successor in function.successor_blocks(node) {
                if !visited.contains(&successor) {
                    this.0
                        .entry(successor)
                        .or_default()
                        .extend(block_opened.iter().cloned());
                    stack.push(successor);
                }
            }
        }
        this
    }

    pub fn find_open(
        &self,
        node: NodeIndex,
        index: usize,
        local: &ast::RcLocal,
        old_locals: &FxHashMap<ast::RcLocal, ast::RcLocal>,
        function: &Function,
    ) -> Option<&ast::RcLocal> {
        let old_local = &old_locals[local];
        self.0[&node]
            .iter()
            .filter(|(open_local, (open_node, _))| {
                &old_locals[open_local] == old_local && *open_node == node
            })
            .find(|(_, (_, open_index))| *open_index < index)
            .map(|(local, _)| local)
            .or_else(|| {
                function.predecessor_blocks(node).find_map(|pred| {
                    self.0[&pred]
                        .iter()
                        .find(|(open_local, _)| &old_locals[open_local] == old_local)
                        .map(|(local, _)| local)
                })
            })
    }
}
