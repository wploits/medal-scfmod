use std::collections::HashMap;
use std::ops::Range;

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
    instruction::{Binary, BinaryOp, ConditionalJump, LoadConstant, Move, UnconditionalJump},
    value::ValueId,
};

pub struct Lifter {
    function: usize,
    function_list: Vec<BytecodeFunction>,
    string_table: Vec<String>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant>,
}

impl Lifter {
    pub fn new(chunk: Chunk) -> Self {
        Lifter {
            function: chunk.main,
            function_list: chunk.functions,
            string_table: chunk.string_table,
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
                &BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                    /*OpCode::LoadBool if c != 0 => {
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }*/
                    /*OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual | OpCode::Test => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                        self.blocks
                            .entry(insn_index + 2)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                    }*/
                    _ => {}
                },

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_JUMP => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize))
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| builder.new_block().unwrap().block_id());
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

    fn convert_constant(&self, constant: &BytecodeConstant) -> Constant {
        match constant {
            BytecodeConstant::Nil => Constant::Nil,
            BytecodeConstant::Boolean(b) => Constant::Boolean(*b),
            BytecodeConstant::Number(n) => Constant::Number(*n),
            BytecodeConstant::String(s_idx) => Constant::String(self.string_table[*s_idx].clone()),
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

    fn get_block(&self, insn_index: usize) -> Option<NodeId> {
        self.blocks.get(&insn_index).cloned()
    }

    fn is_terminator(instruction: &BytecodeInstruction) -> bool {
        match instruction {
            BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                OpCode::LOP_RETURN => true,
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
                OpCode::LOP_JUMPIFNOTLT => true,
                _ => false
            },
            BytecodeInstruction::E { op_code, .. } => matches!(op_code, OpCode::LOP_JUMPX),
        }
    }

    fn lift_block(&mut self, block_start: usize, block_end: usize) -> Result<()> {
        //let mut vararg_index = None;
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
                    OpCode::LOP_MOVE => {
                        let (dest, source) =
                            (self.get_register(a as usize), self.get_register(b as usize));

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(Move { dest, source }.into());
                    }
                    /*OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual => {
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
                    }*/
                    _ => {}
                },

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_LOADK => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(d as usize);

                        let mut builder = Builder::new(&mut self.lifted_function);
                        builder
                            .block(block_index)
                            .unwrap()
                            .push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        println!("LOP_JUMP d {} {} {}", d, instruction_index, instruction_index.wrapping_add(d as usize));
                        let branch = self
                            .get_block(instruction_index.wrapping_add(d as usize))
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
                    _ => {}
                },

                BytecodeInstruction::E { op_code, e } => match op_code {
                    OpCode::LOP_JUMPX => {

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
