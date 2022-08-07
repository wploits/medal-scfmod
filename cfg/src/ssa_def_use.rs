use ast::LocalRw;

use fxhash::FxHashMap;
use graph::{Edge, NodeId};

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
pub struct SsaDefUse {
    pub definitions: FxHashMap<String, Definition>,
    pub parameters: FxHashMap<String, Vec<Edge>>,
    pub references: FxHashMap<String, Vec<Location>>,
}

impl SsaDefUse {
    pub fn new(function: &Function) -> Self {
        let mut this = Self {
            definitions: FxHashMap::default(),
            parameters: FxHashMap::default(),
            references: FxHashMap::default(),
        };
        for (&node, block) in function.blocks() {
            for (index, statement) in block.ast.iter().enumerate() {
                for value in statement.values_written() {
                    this.definitions
                        .insert(value.0.to_string(), Definition::Variable(node, index));
                }
                for value in statement.values_read() {
                    this.references
                        .entry(value.0.to_string())
                        .or_insert_with(Vec::new)
                        .push(Location::Block(node, index));
                }
            }
            match &block.terminator {
                Some(Terminator::Jump(edge)) => {
                    for (target, value) in edge.arguments.iter() {
                        this.parameters
                            .entry(target.0.to_string())
                            .or_insert_with(Vec::new)
                            .push((node, edge.node));
                        this.references
                            .entry(value.0.to_string())
                            .or_insert_with(Vec::new)
                            .push(Location::Edge((node, edge.node)));
                    }
                }
                Some(Terminator::Conditional(then_edge, else_edge)) => {
                    for (target, value) in then_edge.arguments.iter() {
                        this.parameters
                            .entry(target.0.to_string())
                            .or_insert_with(Vec::new)
                            .push((node, then_edge.node));
                        this.references
                            .entry(value.0.to_string())
                            .or_insert_with(Vec::new)
                            .push(Location::Edge((node, then_edge.node)));
                    }
                    for (target, value) in else_edge.arguments.iter() {
                        this.parameters
                            .entry(target.0.to_string())
                            .or_insert_with(Vec::new)
                            .push((node, else_edge.node));
                        this.references
                            .entry(value.0.to_string())
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
