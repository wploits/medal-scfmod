use std::collections::HashMap;
use std::ops::{Range, RangeInclusive};

use anyhow::Result;
use graph::NodeId;

use super::{
    deserializer::chunk::Chunk,
    deserializer::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, deserializer::constant::Constant as BytecodeConstant,
};

use cfg_ir::{
    builder::Builder,
    constant::Constant,
    function::Function,
    instruction::{
        Binary, BinaryOp, ConditionalJump, LoadConstant, Move, UnconditionalJump,
        Unary, UnaryOp, LoadGlobal, StoreGlobal, LoadIndex, Return, Concat
    },
    value::ValueId,
};

pub struct Lifter<'a> {
    function: usize,
    function_list: &'a Vec<BytecodeFunction>,
    string_table: &'a Vec<String>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant>,
}

impl<'a> Lifter<'a> {
    pub fn new(f_list: &'a Vec<BytecodeFunction>, str_list: &'a Vec<String>, function_id: usize) -> Self {
        Lifter {
            function: function_id,
            function_list: f_list,
            string_table: str_list,
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
        for (insn_index, insn) in self.function_list[self.function].instructions.iter().enumerate() {
            match insn {
                BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                    OpCode::LOP_LOADB if *c != 0 => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*c as usize) + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }
                    _ => {}
                }

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }
                    OpCode::LOP_JUMPIF | OpCode::LOP_JUMPIFNOT | 
                    OpCode::LOP_JUMPIFEQ | OpCode::LOP_JUMPIFLE | OpCode::LOP_JUMPIFLT | 
                    OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT |
                    OpCode::LOP_JUMPIFEQK | OpCode::LOP_JUMPIFNOTEQK => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }
                    OpCode::LOP_FORNPREP | OpCode::LOP_FORNLOOP | OpCode::LOP_FORGPREP |
                    OpCode::LOP_FORGLOOP | OpCode::LOP_FORGPREP_INEXT | OpCode::LOP_FORGLOOP_INEXT |
                    OpCode::LOP_FORGLOOP_NEXT => { // unsure how to handle these right now
                        todo!();
                    }
                    _ => {}
                }

                BytecodeInstruction::E { .. } => {}
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

    fn get_register_incl_range(&mut self, range: RangeInclusive<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn convert_constant(&self, constant: &BytecodeConstant) -> Constant {
        match constant {
            BytecodeConstant::Nil => Constant::Nil,
            BytecodeConstant::Boolean(b) => Constant::Boolean(*b),
            BytecodeConstant::Number(n) => Constant::Number(*n),
            BytecodeConstant::String(s_idx) => Constant::String(self.string_table[*s_idx - 1].clone()),
            _ => unimplemented!()
        }
    }

    fn get_constant(&mut self, index: usize) -> Constant {
        let converted_constant = self.convert_constant(self.function_list[self.function].constants.get(index).unwrap());
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn get_block_constant(&mut self, index: usize, block_index: NodeId) -> ValueId {
        let constant = self.get_constant(index);
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
    }

    fn get_block(&self, insn_index: usize) -> Option<NodeId> {
        self.blocks.get(&insn_index).cloned()
    }

    fn is_terminator(instruction: &BytecodeInstruction) -> bool {
        match instruction {
            BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                OpCode::LOP_RETURN => true,
                OpCode::LOP_LOADB if *c != 0 => true,
                _ => false,
            },
            BytecodeInstruction::AD { op_code, .. } => match op_code {
                OpCode::LOP_JUMP |
                OpCode::LOP_JUMPBACK |
                OpCode::LOP_JUMPIF |
                OpCode::LOP_JUMPIFNOT |
                OpCode::LOP_JUMPIFEQ |
                OpCode::LOP_JUMPIFLE |
                OpCode::LOP_JUMPIFLT |
                OpCode::LOP_JUMPIFNOTEQ |
                OpCode::LOP_JUMPIFNOTLE |
                OpCode::LOP_JUMPIFNOTLT |
                OpCode::LOP_JUMPIFEQK | 
                OpCode::LOP_JUMPIFNOTEQK => true,
                OpCode::LOP_FORNPREP | OpCode::LOP_FORNLOOP | OpCode::LOP_FORGPREP |
                OpCode::LOP_FORGLOOP | OpCode::LOP_FORGPREP_INEXT | OpCode::LOP_FORGLOOP_INEXT |
                OpCode::LOP_FORGLOOP_NEXT => true,
                _ => false
            },
            BytecodeInstruction::E { op_code, .. } => matches!(op_code, OpCode::LOP_JUMPX),
        }
    }

    fn lift_block(&mut self, block_start: usize, block_end: usize) -> Result<()> {
        let mut vararg_index = None;
        let mut block_index = self.get_block(block_start).unwrap();
        for (block_instruction_index, instruction) in 
            self.function_list[self.function].instructions[block_start..=block_end]
            .iter()
            .cloned()
            .enumerate()
        {
            let instruction_index = block_start + block_instruction_index;
            match instruction {
                BytecodeInstruction::ABC { op_code, a, b, c, aux } => match op_code {
                    OpCode::LOP_LOADNIL => {
                        let dest = self.get_register(a as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(LoadConstant { dest, constant: Constant::Nil }.into());
                    }
                    OpCode::LOP_LOADB => {
                        let dest = self.get_register(a as usize);
                        if c != 0 {
                            let branch = self.get_block(instruction_index + c as usize + 1).unwrap();
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
                    OpCode::LOP_MOVE => {
                        let (dest, source) =
                            (self.get_register(a as usize), self.get_register(b as usize));

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Move { dest, source }.into());
                    }
                    OpCode::LOP_GETGLOBAL => {
                        let dest = self.get_register(a as usize);
                        let name = self.get_constant(aux as usize);
                        if let Constant::String(name) = name {
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .push(LoadGlobal { dest, name }.into());
                        }
                    }
                    OpCode::LOP_SETGLOBAL => {
                        let value = self.get_register(a as usize);
                        let name = self.get_constant(aux as usize);
                        if let Constant::String(name) = name {
                            let mut builder = Builder::new(&mut self.lifted_function);
                            builder
                                .block(block_index)
                                .unwrap()
                                .push(StoreGlobal { name, value }.into());
                        }
                    }
                    OpCode::LOP_ADD
                    | OpCode::LOP_SUB
                    | OpCode::LOP_MUL
                    | OpCode::LOP_DIV
                    | OpCode::LOP_MOD
                    | OpCode::LOP_POW => {
                        let (dest, lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_register(b as usize),
                            self.get_register(c as usize),
                        );
                        let op = match op_code {
                            OpCode::LOP_ADD => BinaryOp::Add,
                            OpCode::LOP_SUB => BinaryOp::Sub, // and breedable
                            OpCode::LOP_MUL => BinaryOp::Mul,
                            OpCode::LOP_DIV => BinaryOp::Div,
                            OpCode::LOP_MOD => BinaryOp::Mod,
                            OpCode::LOP_POW => BinaryOp::Pow,
                            _ => unreachable!(),
                        };
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Binary { dest, lhs, rhs, op }.into());
                    }
                    OpCode::LOP_ADDK
                    | OpCode::LOP_SUBK
                    | OpCode::LOP_MULK
                    | OpCode::LOP_DIVK
                    | OpCode::LOP_MODK
                    | OpCode::LOP_POWK => {
                        let (dest, lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_register(b as usize),
                            self.get_block_constant(c as usize, block_index),
                        );
                        let op = match op_code {
                            OpCode::LOP_ADDK => BinaryOp::Add,
                            OpCode::LOP_SUBK => BinaryOp::Sub, // and breedable
                            OpCode::LOP_MULK => BinaryOp::Mul,
                            OpCode::LOP_DIVK => BinaryOp::Div,
                            OpCode::LOP_MODK => BinaryOp::Mod,
                            OpCode::LOP_POWK => BinaryOp::Pow,
                            _ => unreachable!(),
                        };
                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Binary { dest, lhs, rhs, op }.into());
                    }
                    OpCode::LOP_CONCAT => {
                        let dest = self.get_register(a as usize);
                        let values = self.get_register_incl_range(b as usize..=c as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Concat { dest, values }.into());
                    }
                    OpCode::LOP_MINUS | OpCode::LOP_NOT | OpCode::LOP_LENGTH => {
                        let (dest, value) =
                            (self.get_register(a as usize), self.get_register(b as usize));
                        let op = match op_code {
                            OpCode::LOP_MINUS => UnaryOp::Minus,
                            OpCode::LOP_NOT => UnaryOp::LogicalNot,
                            OpCode::LOP_LENGTH => UnaryOp::Len,
                            _ => unimplemented!(),
                        };

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Unary { dest, value, op }.into());
                    }
                    OpCode::LOP_RETURN => {
                        let mut values = Vec::new();
                        if b > 1 {
                            values = (a as usize..=(b as usize + a as usize - 2))
                                .map(|v| self.get_register(v as usize))
                                .collect();
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
                    _ => {}
                },

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_LOADN => {
                        let dest = self.get_register(a as usize);
                        let constant = Constant::Number(f64::from(d));

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_LOADK => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(d as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_GETIMPORT => {
                        let dest = self.get_register(a as usize);
                        let count = aux >> 30;
                        let id0 = (aux >> 20) & 1023;
                        let id1 = (aux >> 10) & 1023;
                        let id2 = aux & 1023;

                        if count > 2 {
                            if let Constant::String(name1) = self.get_constant(id0 as usize) {
                                let name2 = self.get_block_constant(id1 as usize, block_index);
                                let name3 = self.get_block_constant(id2 as usize, block_index);
                                let mut builder = Builder::new(&mut self.lifted_function);
                                builder
                                    .block(block_index)
                                    .unwrap()
                                    .push(LoadGlobal { dest, name: name1 }.into())
                                    .push(LoadIndex { dest, object: dest, key: name2 }.into())
                                    .push(LoadIndex { dest, object: dest, key: name3 }.into());
                            }
                        }
                        else if count > 1 {
                            if let Constant::String(name1) = self.get_constant(id0 as usize) {
                                let name2 = self.get_block_constant(id1 as usize, block_index);
                                let mut builder = Builder::new(&mut self.lifted_function);
                                builder
                                    .block(block_index)
                                    .unwrap()
                                    .push(LoadGlobal { dest, name: name1 }.into())
                                    .push(LoadIndex { dest, object: dest, key: name2 }.into());
                            }
                        }
                        else {
                            if let Constant::String(name) = self.get_constant(id0 as usize) {
                                let mut builder = Builder::new(&mut self.lifted_function);
                                builder
                                    .block(block_index)
                                    .unwrap()
                                    .push(LoadGlobal { dest, name }.into());
                            }
                        }
                    }
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        let branch = self
                            .get_block(instruction_index.wrapping_add(d as usize) + 1)
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
                    OpCode::LOP_JUMPIF | OpCode::LOP_JUMPIFNOT => {
                        let condition = self.get_register(a as usize);
                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1).unwrap(),
                            self.get_block(instruction_index + 1).unwrap(),
                        );
                        if matches!(op_code, OpCode::LOP_JUMPIFNOT) {
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
                    OpCode::LOP_JUMPIFEQ | OpCode::LOP_JUMPIFLE | OpCode::LOP_JUMPIFLT |
                    OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT => {
                        let (lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_register(aux as usize),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1).unwrap(),
                            self.get_block(instruction_index + 1).unwrap(),
                        );

                        if matches!(op_code, OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT) {
                            std::mem::swap(&mut true_branch, &mut false_branch);
                        }

                        let op = match op_code {
                            OpCode::LOP_JUMPIFEQ | OpCode::LOP_JUMPIFNOTEQ => BinaryOp::Equal,
                            OpCode::LOP_JUMPIFLE | OpCode::LOP_JUMPIFNOTLE => BinaryOp::LesserThan,
                            OpCode::LOP_JUMPIFLT | OpCode::LOP_JUMPIFNOTLT => BinaryOp::LesserOrEqual,
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
                    OpCode::LOP_JUMPIFEQK | OpCode::LOP_JUMPIFNOTEQK => {
                        let (lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_block_constant(aux as usize, block_index),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index + 1).unwrap(),
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1).unwrap(),
                        );

                        if matches!(op_code, OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT) {
                            //std::mem::swap(&mut true_branch, &mut false_branch);
                        }

                        let op = BinaryOp::Equal;

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
                    _ => {}
                },

                BytecodeInstruction::E { op_code, e } => match op_code {
                    OpCode::LOP_JUMPX => {
                        unimplemented!();
                    }
                    _ => {}
                }
            }
        }

        if !Self::is_terminator(&self.function_list[self.function].instructions[block_end]) {
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
                (self.function_list[self.function].instructions.len(), Vec::new()),
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
