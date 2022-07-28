use std::rc::Rc;

use ast::Local;
use ast::LocalRw;
use by_address::ByAddress;
use fxhash::FxHashMap;
use graph::{algorithms::dfs_tree, Edge, NodeId};

use crate::{block::Terminator, function::Function};

#[derive(Debug)]
pub enum Location {
    Block(NodeId, usize),
    Edge(Edge),
}

#[derive(Debug)]
pub enum Definition {
    Variable(NodeId, usize),
    Parameter(Vec<Edge>),
}

#[derive(Debug, Default)]
pub struct SsaDefUse<'a> {
    pub definitions: FxHashMap<&'a ast::RcLocal<'a>, Definition>,
    pub parameters: FxHashMap<&'a ast::RcLocal<'a>, Vec<Edge>>,
    pub references: FxHashMap<&'a ast::RcLocal<'a>, Vec<Location>>,
}

impl<'a> SsaDefUse<'a> {
    pub fn new(function: &'a Function<'a>) -> Self {
        let mut this = Self {
            definitions: FxHashMap::default(),
            parameters: FxHashMap::default(),
            references: FxHashMap::default(),
        };
        for (&node, block) in function.blocks() {
            for (index, statement) in block.iter().enumerate() {
                for value in statement.values_written() {
                    this.definitions
                        .insert(value, Definition::Variable(node, index));
                }
                for value in statement.values_read() {
                    this.references
                        .entry(value)
                        .or_insert_with(Vec::new)
                        .push(Location::Block(node, index));
                }
            }
            match &block.terminator {
                Some(Terminator::Jump(edge)) => {
                    for (target, value) in edge.arguments.iter() {
                        this.parameters
                            .entry(target)
                            .or_insert_with(Vec::new)
                            .push((node, edge.node));
                        this.references
                            .entry(value)
                            .or_insert_with(Vec::new)
                            .push(Location::Edge((node, edge.node)));
                    }
                }
                Some(Terminator::Conditional(then_edge, else_edge)) => {
                    for (target, value) in then_edge.arguments.iter() {
                        this.parameters
                            .entry(target)
                            .or_insert_with(Vec::new)
                            .push((node, then_edge.node));
                        this.references
                            .entry(value)
                            .or_insert_with(Vec::new)
                            .push(Location::Edge((node, then_edge.node)));
                    }
                    for (target, value) in else_edge.arguments.iter() {
                        this.parameters
                            .entry(target)
                            .or_insert_with(Vec::new)
                            .push((node, else_edge.node));
                        this.references
                            .entry(value)
                            .or_insert_with(Vec::new)
                            .push(Location::Edge((node, else_edge.node)));
                    }
                }
                _ => {}
            }
        }
        this
    }
}
