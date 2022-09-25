use ast::Traverse;
use fxhash::{FxHashMap, FxHashSet};
use petgraph::stable_graph::NodeIndex;

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
pub(crate) struct UpvaluesOpen {
    open: FxHashMap<NodeIndex, FxHashSet<(ast::RcLocal, (NodeIndex, usize))>>,
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
                    block_opened.retain(|(opened, _)| {
                        !statement_closed.contains(&&this.old_locals[opened])
                    });
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
        for statement in function.block(node).unwrap().ast.iter().take(index).rev() {
            for opened in statement_upvalues_opened(statement) {
                let old_opened = &self.old_locals[opened];
                if old_opened == old_local {
                    return true;
                }
            }
            for closed in statement_upvalues_closed(statement) {
                if closed == old_local {
                    return false;
                }
            }
        }

        for pred in function.predecessor_blocks(node) {
            if let Some(pred_open) =
                self.open[&pred]
                    .iter()
                    .find(|(open_local, (open_node, open_index))| {
                        &self.old_locals[open_local] == old_local
                    })
            {
                return true;
            }
        }

        false

        /*self.open[&node]
        .iter()
        .filter(|(open_local, (open_node, _))| {
            &self.old_locals[open_local] == old_local && *open_node == node
        })
        .find(|(_, (_, open_index))| *open_index < index)
        .map(|(local, _)| local)
        .or_else(|| {
            function.predecessor_blocks(node).find_map(|pred| {
                self.open[&pred]
                    .iter()
                    .find(|(open_local, _)| &self.old_locals[open_local] == old_local)
                    .map(|(local, _)| local)
            })
        })*/
    }
}
