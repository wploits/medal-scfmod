use ast::Traverse;
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{algo::dominators::Dominators, stable_graph::NodeIndex, visit::Dfs};

use crate::function::Function;

pub fn statement_upvalues_opened(statement: &ast::Statement) -> Vec<&ast::RcLocal> {
    let mut upvalues_opened = Vec::new();
    for rvalue in statement.rvalues() {
        if let ast::RValue::Closure(closure) = rvalue {
            upvalues_opened.extend(closure.upvalues.iter());
        }
    }
    upvalues_opened
}

pub fn statement_upvalues_closed(statement: &ast::Statement) -> Vec<&ast::RcLocal> {
    if let ast::Statement::Close(close) = statement {
        close.locals.iter().collect()
    } else {
        Vec::new()
    }
}

fn is_open(
    function: &Function,
    node: NodeIndex,
    index: Option<usize>,
    local: &ast::RcLocal,
    dominators: &Dominators<NodeIndex>,
) -> Option<(NodeIndex, usize)> {
    if let Some(index) = index {
        for i in (0..=index).rev() {
            let statement = &function.block(node).unwrap().ast[i];
            if let ast::Statement::Close(close) = statement {
                if close.locals.contains(local) {
                    return None;
                }
            }
            let upvalues_opened = statement_upvalues_opened(statement);
            if upvalues_opened.contains(&local) {
                return Some((node, i));
            }
        }
    }
    if let Some(idom) = dominators.immediate_dominator(node) {
        let ast_len = function.block(idom).unwrap().ast.len();
        if ast_len > 0 {
            is_open(function, idom, Some(ast_len - 1), local, dominators)
        } else {
            is_open(function, idom, None, local, dominators)
        }
    } else {
        None
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Location {
    pub node: NodeIndex,
    pub index: usize,
}

use ast::LocalRw;
use petgraph::algo::dominators;

#[derive(Debug)]
pub struct UpvaluesOpen(FxHashMap<NodeIndex, FxHashSet<(ast::RcLocal, (NodeIndex, usize))>>);

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
                println!("statement: {}", statement);
                let statement_opened = statement_upvalues_opened(statement);
                println!("opened: {:?}", statement_opened);
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

pub fn compute_open_upvalues(
    function: &Function,
) -> FxHashMap<Location, (ast::RcLocal, Vec<Location>)> {
    let mut upvalue_definitions = FxHashMap::default();

    let dominators = dominators::simple_fast(function.graph(), function.entry().unwrap());

    let mut upvalues_opened = FxHashMap::default();
    for (node, block) in function.blocks() {
        for (statement_index, statement) in block.ast.iter().enumerate() {
            if let ast::Statement::Assign(assign) = statement {
                if let ast::RValue::Closure(closure) = &assign.right[0] {
                    for upvalue in &closure.upvalues {
                        upvalues_opened.insert(upvalue, (node, statement_index));
                    }
                }
            }
        }
    }

    for (node, block) in function.blocks() {
        for (index, assign) in
            block
                .ast
                .iter()
                .enumerate()
                .filter_map(|(statement_index, statement)| {
                    statement
                        .as_assign()
                        .map(|assign| (statement_index, assign))
                        .filter(|(_, statement)| {
                            if !(statement.left.len() == 1 && statement.right.len() == 1) {
                                return false;
                            }
                            let values_written = statement.values_written();
                            !values_written.is_empty()
                                && values_written
                                    .iter()
                                    .any(|local| upvalues_opened.contains_key(local))
                        })
                })
        {
            let local = assign.left[0].0.as_local().unwrap();
            if let Some(upvalue_first_def) =
                is_open(function, node, Some(index), local, &dominators)
            {
                upvalue_definitions
                    .entry((local.clone(), upvalue_first_def.0, upvalue_first_def.1))
                    .or_insert_with(Vec::new)
                    .push(Location { node, index });
            }
        }
    }

    upvalue_definitions
        .into_iter()
        .map(|(definition, usages)| {
            (
                Location {
                    node: definition.1,
                    index: definition.2,
                },
                (definition.0, usages),
            )
        })
        .collect()
}
