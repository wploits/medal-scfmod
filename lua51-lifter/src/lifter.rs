use std::{borrow::Cow, cell::RefCell, rc::Rc};

use anyhow::Result;

use cfg::{block::Terminator, function::Function};
use fxhash::FxHashMap;
use graph::NodeId;

use crate::{instruction::Instruction, value};

use super::{
    chunk::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode,
};

pub struct Lifter<'a> {
    bytecode_function: &'a BytecodeFunction<'a>,
    blocks: FxHashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    // TODO: make this a ref
    lifted_descendants: Vec<Rc<RefCell<Function<'a>>>>,
    closures: Vec<Option<Rc<RefCell<Function<'a>>>>>,
    register_map: FxHashMap<usize, Rc<ast::Local<'a>>>,
    constant_map: FxHashMap<usize, ast::Literal<'a>>,
    current_node: Option<NodeId>,
}

impl<'a> Lifter<'a> {
    pub fn new(function: &'a BytecodeFunction<'_>) -> Self {
        Self {
            bytecode_function: function,
            blocks: FxHashMap::default(),
            lifted_function: Function::default(),
            closures: vec![None; function.closures.len()],
            register_map: FxHashMap::default(),
            constant_map: FxHashMap::default(),
            lifted_descendants: Vec::new(),
            current_node: None,
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
                (self.bytecode_function.code.len(), Vec::new()),
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
            )
            .1;

        for i in 0..self.bytecode_function.num_params {
            let parameter = self.lifted_function.local_allocator.allocate();
            self.lifted_function.parameters.push(parameter.clone());
            self.register_map.insert(i as usize, parameter);
        }

        for (start_pc, end_pc) in block_ranges {
            self.current_node = Some(self.block_to_node(start_pc));
            let (statements, terminator) = self.lift_block(start_pc, end_pc);
            let block = &mut self
                .lifted_function
                .block_mut(self.current_node.unwrap())
                .unwrap()
                .block;
            block.extend(statements);
            self.lifted_function
                .set_block_terminator(self.current_node.unwrap(), Some(terminator))
                .unwrap();
        }

        self.lifted_function.set_entry(self.block_to_node(0))?;

        Ok(self.lifted_function)
    }

    fn discover_blocks(&mut self) -> Result<()> {
        self.blocks.insert(0, self.lifted_function.new_block()?);
        for (insn_index, insn) in self.bytecode_function.code.iter().enumerate() {
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

    fn lift_block(
        &mut self,
        block_start: usize,
        block_end: usize,
    ) -> (Vec<ast::Statement<'a>>, Terminator) {
        let mut instructions = Vec::new();
        let mut terminator = None;
        for (index, instruction) in self.bytecode_function.code[block_start..=block_end]
            .iter()
            .enumerate()
        {
            let instruction_index = block_start + index;
            match (instruction.opcode(), instruction) {
                (OpCode::Move, Instruction::ABC { a, b, .. }) => instructions.push(
                    ast::Assign {
                        left: self.register(*a as usize).into(),
                        right: self.register(*b as usize).into(),
                    }
                    .into(),
                ),
                (OpCode::LoadConst, Instruction::ABx { a, bx, .. }) => instructions.push(
                    ast::Assign {
                        left: self.register(*a as usize).into(),
                        right: ast::RValue::Literal(self.constant(*bx as usize)),
                    }
                    .into(),
                ),
                (OpCode::Test, Instruction::ABC { a, b: _, c, .. }) => {
                    let condition = self.register_or_constant(*a as usize, &mut instructions);
                    let (mut true_branch, mut false_branch) = (
                        self.block_to_node(instruction_index + 2),
                        self.block_to_node(instruction_index + 1),
                    );
                    if *c != 0 {
                        std::mem::swap(&mut true_branch, &mut false_branch);
                    }
                    instructions.push(ast::If::new(condition, None, None).into());
                    terminator = Some(Terminator::Conditional(false_branch, true_branch));
                }
                (OpCode::Jump, Instruction::AsBx { sbx, .. }) => {
                    terminator = Some(Terminator::Jump(
                        self.block_to_node(instruction_index + *sbx as usize - 131070),
                    ));
                }
                (OpCode::Return, Instruction::ABC { a: _, c: _, .. }) => {
                    instructions.push(ast::Return::default().into());
                    terminator = Some(Terminator::Return);
                }
                (opcode, _) => unimplemented!("{:?}", opcode),
            }
        }
        (
            instructions,
            terminator.unwrap_or_else(|| Terminator::Jump(self.block_to_node(block_end + 1))),
        )
    }

    fn register_or_constant(
        &mut self,
        index: usize,
        statements: &mut Vec<ast::Statement<'a>>,
    ) -> ast::RValue<'a> {
        if index >= 256 {
            let literal = self.constant(index - 256);
            let local = self.lifted_function.local_allocator.allocate();
            statements.push(ast::Assign::new(local.clone().into(), literal.into()).into());
            local.into()
        } else {
            self.register(index).into()
        }
    }

    fn register(&mut self, index: usize) -> Rc<ast::Local<'a>> {
        self.register_map
            .entry(index)
            .or_insert_with(|| self.lifted_function.local_allocator.allocate())
            .clone()
    }

    fn constant(&mut self, index: usize) -> ast::Literal<'a> {
        let converted_constant = match self.bytecode_function.constants.get(index).unwrap() {
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

    fn block_to_node(&self, insn_index: usize) -> NodeId {
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
