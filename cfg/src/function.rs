use ast::{local_allocator::LocalAllocator, RcLocal};
use contracts::requires;
use itertools::Itertools;
use petgraph::{
    stable_graph::{Neighbors, NodeIndex, StableDiGraph},
    Direction,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::block::{BasicBlock, BasicBlockEdge, Terminator};

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub local_allocator: Rc<RefCell<LocalAllocator>>,
    pub parameters: Vec<RcLocal>,
    pub upvalues_captured: Vec<RcLocal>,
    // TODO: edge data in graph instead of terminator (arguments)
    graph: StableDiGraph<BasicBlock, ()>,
    /*pub upvalue_open_ranges:
    FxHashMap<ValueId, FxHashMap<InstructionLocation, Vec<InstructionLocation>>>,*/
    entry: Option<NodeIndex>,
}

impl Function {
    pub fn with_allocator(local_allocator: Rc<RefCell<LocalAllocator>>) -> Self {
        Self {
            local_allocator,
            parameters: Vec::new(),
            upvalues_captured: Vec::new(),
            graph: StableDiGraph::new(),
            entry: None,
        }
    }

    pub fn entry(&self) -> &Option<NodeIndex> {
        &self.entry
    }

    #[requires(self.has_block(new_entry))]
    pub fn set_entry(&mut self, new_entry: NodeIndex) {
        self.entry = Some(new_entry);
    }

    #[requires(self.has_block(block))]
    pub fn set_block_terminator(
        &mut self,
        block: NodeIndex,
        mut new_terminator: Option<Terminator>,
    ) {
        self.graph
            .retain_edges(|g, e| g.edge_endpoints(e).unwrap().0 != block);
        match &new_terminator {
            Some(Terminator::Jump(edge)) => {
                self.graph.add_edge(block, edge.node, ());
            }
            Some(Terminator::Conditional(then_edge, else_edge)) => {
                self.graph.add_edge(block, then_edge.node, ());
                if then_edge.node != else_edge.node {
                    self.graph.add_edge(block, else_edge.node, ());
                } else {
                    new_terminator = Some(Terminator::jump(then_edge.node));
                }

                /*lif then_edge.node != else_edge.node {
                    self.graph.add_edge(block, else_edge.node, ());
                } else {
                    et source_block = &mut self.block_mut(block).unwrap().ast;
                    let condition = source_block.pop().unwrap().into_if().unwrap().condition;
                    if condition.has_side_effects() {
                        let local = self.local_allocator.borrow_mut().allocate();
                        self.block_mut(block)
                            .unwrap()
                            .ast
                            .push(ast::Assign::new(vec![local.into()], vec![condition]).into());
                    }
                    new_terminator = Some(Terminator::jump(then_edge.node));
                }*/
            }
            _ => {}
        }
        self.block_mut(block).unwrap().terminator = new_terminator;
    }

    #[requires(self.graph.find_edge(source, old_target).is_some())]
    #[requires(self.has_block(target))]
    pub fn replace_edge(&mut self, source: NodeIndex, old_target: NodeIndex, target: NodeIndex) {
        if self.successor_blocks(source).contains(&target) {
            self.set_block_terminator(source, Some(Terminator::jump(target)));
        } else {
            self.graph
                .remove_edge(self.graph.find_edge(source, old_target).unwrap());
            self.graph.add_edge(source, target, ());
            self.block_mut(source)
                .unwrap()
                .terminator
                .as_mut()
                .unwrap()
                .replace_branch(old_target, target);
        }
    }

    // TODO: take EdgeIndex as argument
    pub fn remove_edge(&mut self, source: NodeIndex, target: NodeIndex) {
        self.graph
            .remove_edge(self.graph.find_edge(source, target).unwrap());
    }

    pub fn graph(&self) -> &StableDiGraph<BasicBlock, ()> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut StableDiGraph<BasicBlock, ()> {
        &mut self.graph
    }

    pub fn has_block(&self, block: NodeIndex) -> bool {
        self.graph.contains_node(block)
    }

    pub fn block(&self, block: NodeIndex) -> Option<&BasicBlock> {
        self.graph.node_weight(block)
    }

    pub fn block_mut(&mut self, block: NodeIndex) -> Option<&mut BasicBlock> {
        self.graph.node_weight_mut(block)
    }

    pub fn blocks(&self) -> impl Iterator<Item = (NodeIndex, &BasicBlock)> {
        self.graph
            .node_indices()
            .map(|i| (i, self.graph.node_weight(i).unwrap()))
    }

    pub fn blocks_mut(&mut self) -> HashMap<NodeIndex, &mut BasicBlock> {
        self.graph
            .node_indices()
            .collect_vec()
            .into_iter()
            .zip(self.graph.node_weights_mut())
            .collect()
    }

    pub fn successor_blocks(&self, block: NodeIndex) -> Neighbors<()> {
        self.graph.neighbors_directed(block, Direction::Outgoing)
    }

    pub fn predecessor_blocks(&self, block: NodeIndex) -> Neighbors<()> {
        self.graph.neighbors_directed(block, Direction::Incoming)
    }

    pub fn edges_to_block(&self, node: NodeIndex) -> Vec<(NodeIndex, &BasicBlockEdge)> {
        self.predecessor_blocks(node)
            .flat_map(|p| {
                self.graph
                    .node_weight(p)
                    .unwrap()
                    .terminator
                    .as_ref()
                    .map_or_else(Vec::new, |t| {
                        t.edges().into_iter().map(|e| (p, e)).collect()
                    })
            })
            .filter(|(_, e)| e.node == node)
            .collect::<Vec<_>>()
    }

    pub fn edges_to_block_mut(&mut self, node: NodeIndex) -> Vec<&mut BasicBlockEdge> {
        self.graph
            .node_weights_mut()
            .filter_map(|w| w.terminator.as_mut())
            .flat_map(|t| t.edges_mut())
            .filter(|e| e.node == node)
            .collect::<Vec<_>>()
    }

    pub fn new_block(&mut self) -> NodeIndex {
        self.graph.add_node(BasicBlock::default())
    }

    pub fn remove_block(&mut self, block: NodeIndex) -> Option<BasicBlock> {
        self.graph.remove_node(block)
    }
}
