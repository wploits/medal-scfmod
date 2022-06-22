use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap},
    iter,
    ops::{Bound, Range, RangeInclusive},
    rc::Rc,
};

use anyhow::Result;

use cfg_ir::{
    function::Function,
    value::ValueId,
    value_allocator::ValueAllocator,
};
use fxhash::{FxHashMap, FxHashSet};
use graph::{
    algorithms::{
        dfs_tree,
        dominators::{common_dominator, compute_immediate_dominators, dominators},
    },
    NodeId,
};

use crate::instruction::Instruction;
use crate::value;

use super::{
    chunk::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, value::ValueId as BytecodeValueId,
};

pub struct Lifter<'a> {
    function: &'a BytecodeFunction<'a>,
    blocks: FxHashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    // TODO: make this a ref
    lifted_descendants: Vec<Rc<RefCell<Function<'a>>>>,
    closures: Vec<Option<Rc<RefCell<Function<'a>>>>>,
    register_map: FxHashMap<usize, ValueId>,
    constant_map: FxHashMap<usize, ast::Literal<'a>>,
}

impl<'a> Lifter<'a> {
    pub fn new(
        function: &'a BytecodeFunction<'_>,
        value_allocator: Rc<RefCell<ValueAllocator>>,
    ) -> Self {
        Self {
            function,
            blocks: FxHashMap::default(),
            lifted_function: Function::new(value_allocator),
            closures: vec![None; function.closures.len()],
            register_map: FxHashMap::default(),
            constant_map: FxHashMap::default(),
            lifted_descendants: Vec::new(),
        }
    }

    pub fn lift_function(mut self) -> Result<Function<'a>> {
        self.discover_blocks()?;

        let mut blocks = self.blocks.keys().cloned().collect::<Vec<_>>();

        blocks.sort_unstable();

        let block_ranges = blocks
            .iter()
            .rev()
            .fold(
                (self.function.code.len(), Vec::new()),
                |(block_end, mut accumulator), &block_start| {
                    accumulator.push((block_start, block_end - 1));

                    (
                        if block_start != 0 {
                            block_start
                        } else {
                            block_end
                        },
                        accumulator,
                    )
                },
            ).1;

        for i in 0..self.function.num_params {
            let parameter = self
                .lifted_function
                .value_allocator
                .borrow_mut()
                .new_value();
            self.lifted_function.parameters.push(parameter);
            self.register_map.insert(i as usize, parameter);
        }

        let mut node_to_block =
            FxHashMap::with_capacity_and_hasher(block_ranges.len(), Default::default());

        for (block_start_pc, block_end_pc) in block_ranges {
            let cfg_block_id = self.get_block(block_start_pc);

            node_to_block.insert(cfg_block_id, (block_start_pc, block_end_pc));

            let instructions = self.lift_instructions(block_start_pc, block_end_pc);
            let block = self.lifted_function.block_mut(cfg_block_id).unwrap();

            block.statements.extend(instructions);

            //self.lifted_function
            //  .set_block_terminator(cfg_block_id, terminator)?;
        }

        self.lifted_function.set_entry(self.get_block(0))?;

        let dfs = dfs_tree(
            self.lifted_function.graph(),
            self.lifted_function.entry().unwrap(),
        )?;
        let idoms = compute_immediate_dominators(
            self.lifted_function.graph(),
            self.lifted_function.entry().unwrap(),
            &dfs,
        )?;
        let dominators = dominators(
            self.lifted_function.graph(),
            self.lifted_function.entry().unwrap(),
            &idoms,
        ).unwrap();

        Ok(self.lifted_function)
    }

    fn discover_blocks(&mut self) -> Result<()> {
        self.blocks.insert(0, self.lifted_function.new_block()?);
        for (insn_index, insn) in self.function.code.iter().enumerate() {
            match *insn {
                BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                    OpCode::SetList if c == 0 => {
                        // TODO: skip next instruction
                        todo!();
                    }
                    OpCode::LoadBool if c != 0 => {
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::Equal
                    | OpCode::LesserThan
                    | OpCode::LesserOrEqual
                    | OpCode::Test
                    | OpCode::TableForLoop => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    _ => {}
                },

                BytecodeInstruction::ABx { .. } => {}

                BytecodeInstruction::AsBx { op_code, a: _, sbx }
                if matches!(op_code, OpCode::Jump | OpCode::ForLoop | OpCode::ForPrep) =>
                    {
                        self.blocks
                            .entry(insn_index + sbx as usize - 131070)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                _ => {}
            }
        }

        Ok(())
    }

    fn lift_instructions(&mut self, block_start: usize, block_end: usize) -> Vec<ast::Statement> {
        let mut instructions = Vec::new();

        for instruction in &self.function.code[block_start..=block_end] {
            match (instruction.opcode(), instruction) {
                (OpCode::LoadConst, Instruction::ABC { a, b, .. }) => {
                    instructions.push(
                        ast::Assign {
                            left: ast::Local(
                                Cow::Owned(
                                    self.get_register(*a as usize)
                                        .to_string()
                                )
                            ).into(),
                            right: ast::RValue::Literal(self.get_constant(*b as usize)),
                        }.into()
                    )
                }
                (opcode, _) => unimplemented!("{:?}", opcode),
            }
        }

        instructions
    }

    fn get_register(&mut self, index: usize) -> ValueId {
        *self.register_map.entry(index).or_insert_with(|| {
            self.lifted_function
                .value_allocator
                .borrow_mut()
                .new_value()
        })
    }

    fn get_register_range(&mut self, range: Range<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn get_register_incl_range(&mut self, range: RangeInclusive<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn get_constant(&mut self, index: usize) -> ast::Literal<'a> {
        let converted_constant = match self.function.constants.get(index).unwrap() {
            value::ValueId::Nil => ast::Literal::Nil,
            value::ValueId::Boolean(v) => ast::Literal::Boolean(*v),
            value::ValueId::Number(v) => ast::Literal::Number(*v),
            value::ValueId::String(v) => ast::Literal::String(Cow::Borrowed(v)),
        };
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn get_block(&self, insn_index: usize) -> NodeId {
        *self.blocks.get(&insn_index).unwrap()
    }

    fn is_terminator(instruction: &BytecodeInstruction) -> bool {
        match instruction {
            BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                OpCode::LoadBool => *c != 0,
                _ => matches!(
                    op_code,
                    OpCode::Equal
                        | OpCode::LesserThan
                        | OpCode::LesserOrEqual
                        | OpCode::Test
                        | OpCode::Return
                        | OpCode::TableForLoop
                ),
            },
            BytecodeInstruction::ABx { .. } => false,
            BytecodeInstruction::AsBx { op_code, .. } => {
                matches!(op_code, OpCode::Jump | OpCode::ForPrep | OpCode::ForLoop)
            }
        }
    }
}