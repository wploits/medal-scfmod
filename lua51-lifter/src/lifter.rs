use std::collections::HashMap;
use std::ops::Range;

use anyhow::Result;
use graph::NodeId;

use super::{
    chunk::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, value::ValueId as BytecodeValueId,
};

use cfg_ir::{
    builder::Builder,
    constant::Constant,
    function::Function,
    instruction::{
        Binary, BinaryOp, ConditionalJump, LoadConstant, LoadGlobal, Move, Return, StoreGlobal,
        UnconditionalJump,
    },
    value::ValueId,
};

pub struct Lifter<'a> {
    function: &'a BytecodeFunction<'a>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant>,
}

impl<'a> Lifter<'a> {
    pub fn new(function: &'a BytecodeFunction<'_>) -> Self {
        Lifter {
            function,
            blocks: HashMap::new(),
            lifted_function: Function::new(),
            register_map: HashMap::new(),
            constant_map: HashMap::new(),
        }
    }

    fn discover_blocks(&mut self) -> Result<()> {
        let mut builder = Builder::new(&mut self.lifted_function);
        self.blocks
            .insert(0, builder.new_block()?.mark_entry().block_id());
        for (insn_index, insn) in self.function.code.iter().enumerate() {
            match insn {
                &BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                    OpCode::LoadBool if c != 0 => {
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }
                    OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual | OpCode::Test => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }
                    _ => {}
                },

                &BytecodeInstruction::ABx { .. } => {}

                &BytecodeInstruction::AsBx { op_code, a: _, sbx } if op_code == OpCode::Jump => {
                    self.blocks
                        .entry(insn_index + sbx as usize - 131070)
                        .or_insert_with(|| builder.new_block().unwrap().block_id());
                    self.blocks
                        .entry(insn_index + 1)
                        .or_insert_with(|| builder.new_block().unwrap().block_id());
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn get_register(&mut self, index: usize) -> ValueId {
        let mut builder = Builder::new(&mut self.lifted_function);
        *self
            .register_map
            .entry(index)
            .or_insert_with(|| builder.new_value())
    }

    fn get_register_range(&mut self, range: Range<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn convert_constant(&self, constant: &BytecodeValueId) -> Constant {
        match constant {
            BytecodeValueId::Nil => Constant::Nil,
            BytecodeValueId::Boolean(b) => Constant::Boolean(*b),
            BytecodeValueId::Number(n) => Constant::Number(*n),
            BytecodeValueId::String(s) => Constant::String(s.to_owned().to_string()),
        }
    }

    fn get_constant(&mut self, index: usize) -> Constant {
        let converted_constant = self.convert_constant(self.function.constants.get(index).unwrap());
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn get_block(&self, insn_index: usize) -> Option<NodeId> {
        self.blocks.get(&insn_index).cloned()
    }

    fn get_register_or_constant(&mut self, index: usize, block_index: NodeId) -> ValueId {
        if index >= 256 {
            let constant = self.get_constant(index - 256);
            let mut builder = Builder::new(&mut self.lifted_function);
            let value = builder.new_value();
            builder.block(block_index).unwrap().push(
                LoadConstant {
                    dest: value,
                    constant,
                }
                .into(),
            );
            value
        } else {
            self.get_register(index)
        }
    }

    fn is_terminator(instruction: &BytecodeInstruction) -> bool {
        match instruction {
            BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                OpCode::Equal
                | OpCode::LesserThan
                | OpCode::LesserOrEqual
                | OpCode::Test
                | OpCode::Return => true,
                OpCode::LoadBool => *c != 0,
                _ => false,
            },
            BytecodeInstruction::ABx { .. } => false,
            BytecodeInstruction::AsBx { op_code, .. } => matches!(op_code, OpCode::Jump),
        }
    }

    fn lift_block(&mut self, block_start: usize, block_end: usize) -> Result<()> {
        let mut vararg_index = None;
        let mut block_index = self.get_block(block_start).unwrap();
        for (block_instruction_index, instruction) in self.function.code[block_start..=block_end]
            .iter()
            .enumerate()
        {
            let instruction_index = block_start + block_instruction_index;
            match *instruction {
                BytecodeInstruction::ABC { op_code, a, b, c } => match op_code {
                    OpCode::Move => {
                        let (dest, source) =
                            (self.get_register(a as usize), self.get_register(b as usize));

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Move { dest, source }.into());
                    }
                    OpCode::LoadBool => {
                        let dest = self.get_register(a as usize);
                        if c != 0 {
                            let branch = self.get_block(instruction_index + 2).unwrap();
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .push(
                                    LoadConstant {
                                        dest,
                                        constant: Constant::Boolean(b != 0),
                                    }
                                    .into(),
                                )
                                .replace_terminator(UnconditionalJump(branch).into())
                                .unwrap();
                            if instruction_index != block_end {
                                block_index = builder.new_block()?.block_id()
                            }
                        } else {
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder.block(block_index).unwrap().push(
                                LoadConstant {
                                    dest,
                                    constant: Constant::Boolean(b != 0),
                                }
                                .into(),
                            );
                        }
                    }
                    /*OpCode::LoadNil => {
                        for i in a as u16..=b {
                            let dest = self.get_register(i as usize);
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .load_constant(dest, Constant::Nil)
                                .unwrap();
                        }
                    }
                    OpCode::Index => {
                        let dest = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.get_register_or_constant(c as usize, block_index);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .load_index(dest, object, key)
                            .unwrap();
                    }
                    OpCode::NewIndex => {
                        let object = self.get_register(a as usize);
                        let key = self.get_register_or_constant(b as usize, block_index);
                        let value = self.get_register_or_constant(c as usize, block_index);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .store_index(object, key, value)
                            .unwrap();
                    }
                    OpCode::NewTable => {
                        let dest = self.get_register(a as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .load_table(dest)
                            .unwrap();
                    }*/
                    OpCode::Add
                    | OpCode::Sub
                    | OpCode::Mul
                    | OpCode::Div
                    | OpCode::Mod
                    | OpCode::Pow => {
                        let (dest, lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_register_or_constant(b as usize, block_index),
                            self.get_register_or_constant(c as usize, block_index),
                        );
                        let op = match op_code {
                            OpCode::Add => BinaryOp::Add,
                            OpCode::Sub => BinaryOp::Sub, // and breedable
                            OpCode::Mul => BinaryOp::Mul,
                            OpCode::Div => BinaryOp::Div,
                            OpCode::Mod => BinaryOp::Mod,
                            OpCode::Pow => BinaryOp::Pow,
                            _ => unreachable!(),
                        };
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Binary { dest, lhs, rhs, op }.into());
                    }
                    /*OpCode::UnaryMinus | OpCode::Not | OpCode::Len => {
                        let (dest, value) =
                            (self.get_register(a as usize), self.get_register(b as usize));
                        let op = match op_code {
                            OpCode::UnaryMinus => UnaryOp::Minus,
                            OpCode::Not => UnaryOp::Not,
                            OpCode::Len => UnaryOp::Len,
                            _ => unimplemented!(),
                        };

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .unary(dest, value, op)
                            .unwrap();
                    }*/
                    OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual => {
                        let (lhs, rhs) = (
                            self.get_register_or_constant(b as usize, block_index),
                            self.get_register_or_constant(c as usize, block_index),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index + 2).unwrap(),
                            self.get_block(instruction_index + 1).unwrap(),
                        );

                        if a != 0 {
                            std::mem::swap(&mut true_branch, &mut false_branch);
                        }

                        let op = match op_code {
                            OpCode::Equal => BinaryOp::Equal,
                            OpCode::LesserThan => BinaryOp::LesserThan,
                            OpCode::LesserOrEqual => BinaryOp::LesserOrEqual,
                            _ => panic!(),
                        };

                        let mut builder = Builder::new(&mut self.lifted_function);
                        let condition = builder.new_value();
                        builder
                            .block(block_index)?
                            .push(
                                Binary {
                                    dest: condition,
                                    lhs,
                                    rhs,
                                    op,
                                }
                                .into(),
                            )
                            .replace_terminator(
                                ConditionalJump {
                                    condition,
                                    true_branch,
                                    false_branch,
                                }
                                .into(),
                            )?;
                        if instruction_index != block_end {
                            block_index = builder.new_block()?.block_id();
                        }
                    }
                    OpCode::Test => {
                        let condition = self.get_register_or_constant(a as usize, block_index);
                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index + 2).unwrap(),
                            self.get_block(instruction_index + 1).unwrap(),
                        );
                        if c != 0 {
                            std::mem::swap(&mut true_branch, &mut false_branch);
                        }
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder.block(block_index)?.replace_terminator(
                            ConditionalJump {
                                condition,
                                true_branch,
                                false_branch,
                            }
                            .into(),
                        )?;
                        if instruction_index != block_end {
                            block_index = builder.new_block()?.block_id();
                        }
                    }
                    /*OpCode::Call | OpCode::TailCall => {
                        let function = self.get_register(a as usize);
                        let mut arguments = (a as u16 + 1..a as u16 + b)
                            .map(|v| self.get_register(v as usize))
                            .collect::<Vec<_>>();
                        let return_values = (a as u16..a as u16 + c - 1)
                            .map(|v| self.get_register(v as usize))
                            .collect::<Vec<_>>();
                        if b == 0 {
                            assert!(vararg_index.is_some());
                            arguments =
                                self.get_register_range(a as usize + 1..vararg_index.unwrap());
                        }
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .call(function, arguments, return_values, b == 0, c == 0)
                            .unwrap();
                    }*/
                    OpCode::Return => {
                        let mut values = Vec::new();
                        if b > 1 {
                            values = (a as usize..=b as usize)
                                .map(|v| self.get_register(v as usize))
                                .collect();
                            println!("{:?}", values);
                        }
                        if b == 0 {
                            assert!(vararg_index.is_some());
                            values = self.get_register_range(a as usize..vararg_index.unwrap());
                        }
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder.block(block_index).unwrap().replace_terminator(
                            Return {
                                values,
                                variadic: b == 0,
                            }
                            .into(),
                        )?;
                        if instruction_index != block_end {
                            block_index = builder.new_block()?.block_id();
                        }
                        break;
                    }
                    /*OpCode::VarArg => {
                        vararg_index = Some(a as usize);
                    }*/
                    _ => {}
                },

                BytecodeInstruction::ABx { op_code, a, bx } => match op_code {
                    OpCode::LoadConst => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(bx as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::GetGlobal => {
                        let dest = self.get_register(a as usize);
                        let name = self.get_constant(bx as usize);
                        if let Constant::String(name) = name {
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .push(LoadGlobal { dest, name }.into());
                        }
                    }
                    OpCode::SetGlobal => {
                        let value = self.get_register(a as usize);
                        let name = self.get_constant(bx as usize);
                        if let Constant::String(name) = name {
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .push(StoreGlobal { name, value }.into());
                        }
                    }
                    _ => {}
                },

                BytecodeInstruction::AsBx { op_code, a, sbx } => {
                    if matches!(op_code, OpCode::Jump) {
                        let branch = self
                            .get_block(instruction_index + sbx as usize - 131070)
                            .unwrap();
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .replace_terminator(UnconditionalJump(branch).into())?;
                        if instruction_index != block_end {
                            let new_block_index = builder.new_block()?.block_id();

                            block_index = new_block_index
                        }
                    }
                }
            }
        }

        if !Self::is_terminator(&self.function.code[block_end]) {
            let branch = self.get_block(block_end + 1).unwrap();
            let mut builder = Builder::new(&mut self.lifted_function);
            builder
                .block(block_index)
                .unwrap()
                .replace_terminator(UnconditionalJump(branch).into())?;
        }

        Ok(())
    }

    pub fn lift_function(&mut self) -> Result<Function> {
        self.discover_blocks()?;

        let mut blocks = self.blocks.keys().cloned().collect::<Vec<_>>();
        blocks.sort_unstable();

        //TODO: rewrite, this is chinese
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
            )
            .1;

        for (block_start, block_end) in block_ranges {
            self.lift_block(block_start, block_end)?;
        }

        let mut function = self.lifted_function.clone();
        function.graph_mut().set_entry(self.get_block(0).unwrap())?;

        //function.remove_unconnected_blocks().unwrap();
        Ok(function)
    }
}
