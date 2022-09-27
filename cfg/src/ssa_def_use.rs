use std::any::Any;

use ast::{LocalRw, RcLocal};

use fxhash::FxHashMap;
use petgraph::stable_graph::NodeIndex;

use crate::{block::Terminator, function::Function};

#[derive(Debug)]
pub enum Location {
    Block(NodeIndex, usize),
    Edge((NodeIndex, NodeIndex)),
}

#[derive(Debug)]
pub enum Definition {
    Variable(NodeIndex, usize),
    Parameter(Vec<(NodeIndex, NodeIndex)>),
}

#[derive(Debug, Default)]
pub struct SsaDefUse {
    // TODO: one hash map to a structure containing the stuff
    pub definitions: FxHashMap<RcLocal, Definition>,
    pub parameters: FxHashMap<RcLocal, Vec<(NodeIndex, NodeIndex)>>,
    pub references: FxHashMap<RcLocal, Vec<Location>>,
}

impl SsaDefUse {
    pub fn new(function: &Function) -> Self {
        let mut this = Self {
            definitions: FxHashMap::default(),
            parameters: FxHashMap::default(),
            references: FxHashMap::default(),
        };
        for (node, block) in function.blocks() {
            for (index, statement) in block.ast.iter().enumerate() {
                for value in statement.values_written() {
                    this.definitions
                        .insert(value.clone(), Definition::Variable(node, index));
                }
                for value in statement.values_read() {
                    this.references
                        .entry(value.clone())
                        .or_default()
                        .push(Location::Block(node, index));
                }
            }
            match &block.terminator {
                Some(Terminator::Jump(edge)) => {
                    for (target, value) in edge.arguments.iter() {
                        this.parameters
                            .entry(target.clone())
                            .or_default()
                            .push((node, edge.node));
                        this.references
                            .entry(value.clone())
                            .or_default()
                            .push(Location::Edge((node, edge.node)));
                    }
                }
                Some(Terminator::Conditional(then_edge, else_edge)) => {
                    for (target, value) in then_edge.arguments.iter() {
                        this.parameters
                            .entry(target.clone())
                            .or_default()
                            .push((node, then_edge.node));
                        this.references
                            .entry(value.clone())
                            .or_default()
                            .push(Location::Edge((node, then_edge.node)));
                    }
                    for (target, value) in else_edge.arguments.iter() {
                        this.parameters
                            .entry(target.clone())
                            .or_default()
                            .push((node, else_edge.node));
                        this.references
                            .entry(value.clone())
                            .or_default()
                            .push(Location::Edge((node, else_edge.node)));
                    }
                }
                _ => {}
            }
        }
        this
    }
}
