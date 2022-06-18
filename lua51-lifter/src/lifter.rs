use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::{Range, RangeInclusive};
use std::rc::Rc;

use anyhow::Result;

use cfg_ir::instruction::{
    Call, Closure, Inner, LoadIndex, NumericFor, StoreIndex, Terminator,
};
use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{
        Binary, BinaryOp, Concat, ConditionalJump, LoadConstant, LoadGlobal, Move, Return,
        StoreGlobal, Unary, UnaryOp, UnconditionalJump,
    },
    value::ValueId,
};
use graph::NodeId;

use super::{
    chunk::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, value::ValueId as BytecodeValueId,
};

pub struct Lifter<'a> {
    function: &'a BytecodeFunction<'a>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    closures: Vec<Option<Rc<Function<'a>>>>,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant<'a>>,
}

impl<'a> Lifter<'a> {
    pub fn new(function: &'a BytecodeFunction<'_>) -> Self {
        Self {
            function,
            blocks: HashMap::new(),
            lifted_function: Function::new(),
            closures: vec![None; function.closures.len()],
            register_map: HashMap::new(),
            constant_map: HashMap::new(),
        }
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
                    OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual | OpCode::Test => {
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

    fn get_register(&mut self, index: usize) -> ValueId {
        *self
            .register_map
            .entry(index)
            .or_insert_with(|| self.lifted_function.new_value())
    }

    fn get_register_range(&mut self, range: Range<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn get_register_incl_range(&mut self, range: RangeInclusive<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn convert_constant(&self, constant: &'a BytecodeValueId) -> Constant<'a> {
        match *constant {
            BytecodeValueId::Nil => Constant::Nil,
            BytecodeValueId::Boolean(b) => Constant::Boolean(b),
            BytecodeValueId::Number(n) => Constant::Number(n),
            BytecodeValueId::String(s) => Constant::String(Cow::Borrowed(s)),
        }
    }

    fn get_constant(&mut self, index: usize) -> Constant<'a> {
        let converted_constant = self.convert_constant(self.function.constants.get(index).unwrap());
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn get_block(&self, insn_index: usize) -> NodeId {
        *self.blocks.get(&insn_index).unwrap()
    }

    fn get_register_or_constant(&mut self, index: usize, block_index: NodeId) -> ValueId {
        if index >= 256 {
            let constant = self.get_constant(index - 256);
            let value = self.lifted_function.new_value();
            self.lifted_function
                .block_mut(block_index)
                .unwrap()
                .inner_instructions
                .push(
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
                OpCode::LoadBool => *c != 0,
                _ => matches!(
                    op_code,
                    OpCode::Equal
                        | OpCode::LesserThan
                        | OpCode::LesserOrEqual
                        | OpCode::Test
                        | OpCode::Return
                ),
            },
            BytecodeInstruction::ABx { .. } => false,
            BytecodeInstruction::AsBx { op_code, .. } => {
                matches!(op_code, OpCode::Jump | OpCode::ForPrep | OpCode::ForLoop)
            }
        }
    }

    fn lift_instructions(
        &mut self,
        block_start: usize,
        block_end: usize,
        cfg_block_id: NodeId,
    ) -> Result<(Vec<Inner<'a>>, Option<Terminator>)> {
        let mut instructions = Vec::new();
        let mut terminator = None;
        let mut top_index = None;
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
                        instructions.push(Move { dest, source }.into());
                    }
                    OpCode::LoadBool => {
                        let dest = self.get_register(a as usize);
                        if c != 0 {
                            let branch = self.get_block(instruction_index + 2);
                            instructions.push(
                                LoadConstant {
                                    dest,
                                    constant: Constant::Boolean(b != 0),
                                }
                                .into(),
                            );
                            terminator = Some(UnconditionalJump(branch).into());
                        } else {
                            instructions.push(
                                LoadConstant {
                                    dest,
                                    constant: Constant::Boolean(b != 0),
                                }
                                .into(),
                            );
                        }
                    }
                    OpCode::LoadNil => {
                        for i in a as u16..=b {
                            let dest = self.get_register(i as usize);
                            instructions.push(
                                LoadConstant {
                                    dest,
                                    constant: Constant::Nil,
                                }
                                .into(),
                            );
                        }
                    }
                    OpCode::Index => {
                        let dest = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.get_register_or_constant(c as usize, cfg_block_id);

                        instructions.push(LoadIndex { dest, object, key }.into());
                    }
                    OpCode::NewIndex => {
                        let object = self.get_register(a as usize);
                        let key = self.get_register_or_constant(b as usize, cfg_block_id);
                        let value = self.get_register_or_constant(c as usize, cfg_block_id);

                        instructions.push(StoreIndex { value, object, key }.into());
                    }
                    /*OpCode::NewTable => {
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
                            self.get_register_or_constant(b as usize, cfg_block_id),
                            self.get_register_or_constant(c as usize, cfg_block_id),
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
                        instructions.push(Binary { dest, lhs, rhs, op }.into());
                    }
                    OpCode::UnaryMinus | OpCode::Not | OpCode::Len => {
                        let (dest, value) =
                            (self.get_register(a as usize), self.get_register(b as usize));
                        let op = match op_code {
                            OpCode::UnaryMinus => UnaryOp::Minus,
                            OpCode::Not => UnaryOp::LogicalNot,
                            OpCode::Len => UnaryOp::Len,
                            _ => unimplemented!(),
                        };
                        instructions.push(Unary { dest, value, op }.into());
                    }
                    OpCode::Concat => {
                        let dest = self.get_register(a as usize);
                        let values = self.get_register_incl_range(b as usize..=c as usize);
                        instructions.push(Concat { dest, values }.into());
                    }
                    OpCode::Equal | OpCode::LesserThan | OpCode::LesserOrEqual => {
                        let (lhs, rhs) = (
                            self.get_register_or_constant(b as usize, cfg_block_id),
                            self.get_register_or_constant(c as usize, cfg_block_id),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index + 2),
                            self.get_block(instruction_index + 1),
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

                        let condition = self.lifted_function.new_value();
                        instructions.push(
                            Binary {
                                dest: condition,
                                lhs,
                                rhs,
                                op,
                            }
                            .into(),
                        );
                        terminator = Some(
                            ConditionalJump {
                                condition,
                                true_branch,
                                false_branch,
                            }
                            .into(),
                        );
                    }
                    OpCode::Test => {
                        let condition = self.get_register_or_constant(a as usize, cfg_block_id);
                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index + 2),
                            self.get_block(instruction_index + 1),
                        );
                        if c != 0 {
                            std::mem::swap(&mut true_branch, &mut false_branch);
                        }
                        terminator = Some(
                            ConditionalJump {
                                condition,
                                true_branch,
                                false_branch,
                            }
                            .into(),
                        );
                    }
                    OpCode::Call | OpCode::TailCall => {
                        let function = self.get_register(a as usize);
                        let mut arguments = (a as u16 + 1..a as u16 + b)
                            .map(|v| self.get_register(v as usize))
                            .collect::<Vec<_>>();
                        let return_values = (a as u16..a as u16 + c - 1)
                            .map(|v| self.get_register(v as usize))
                            .collect::<Vec<_>>();
                        if b == 0 {
                            assert!(top_index.is_some());
                            arguments = self.get_register_range(a as usize + 1..top_index.unwrap());
                        }
                        if c == 0 {
                            top_index = Some(a as usize);
                        }
                        instructions.push(
                            Call {
                                function,
                                arguments,
                                variadic_arguments: b == 0,
                                return_values,
                                variadic_return: c == 0,
                            }
                            .into(),
                        );
                    }
                    OpCode::Return => {
                        let mut values = Vec::new();
                        if b > 1 {
                            values = (a as usize..=(b as usize + a as usize - 2))
                                .map(|v| self.get_register(v as usize))
                                .collect();
                        }
                        if b == 0 {
                            assert!(top_index.is_some());
                            values = self.get_register_range(a as usize..top_index.unwrap());
                        }
                        terminator = Some(
                            Return {
                                values,
                                variadic: b == 0,
                            }
                            .into(),
                        );
                        break;
                    }
                    /*OpCode::VarArg => {
                        vararg_index = Some(a as usize);
                    }*/
                    _ => unreachable!(),
                },

                BytecodeInstruction::ABx { op_code, a, bx } => match op_code {
                    OpCode::LoadConst => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(bx as usize);

                        instructions.push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::GetGlobal => {
                        let dest = self.get_register(a as usize);
                        let name = self.get_constant(bx as usize);
                        if let Constant::String(name) = name {
                            instructions.push(LoadGlobal { dest, name }.into());
                        }
                    }
                    OpCode::SetGlobal => {
                        let value = self.get_register(a as usize);
                        let name = self.get_constant(bx as usize);
                        if let Constant::String(name) = name {
                            instructions.push(StoreGlobal { name, value }.into());
                        }
                    }
                    OpCode::Closure => {
                        let dest = self.get_register(a as usize);

                        let child = if let Some(child) = &self.closures[bx as usize] {
                            child.clone()
                        } else {
                            let child = Lifter::new(&self.function.closures[bx as usize])
                                .lift_function()
                                .map(Rc::new)?;
                            self.closures[bx as usize] = Some(child.clone());
                            child
                        };

                        instructions.push(Inner::Closure(Closure {
                            dest,
                            function: child,
                        }));
                    }
                    _ => unreachable!(),
                },

                // TODO: BytecodeInstruction::AsBx { OpCode::Jump, a, sbx, }, etc.
                // so can be in same order as OpCode enum :)
                BytecodeInstruction::AsBx { op_code, a, sbx } => {
                    match op_code {
                        OpCode::ForPrep => {
                            let init = self.get_register(a as usize);
                            let limit = self.get_register(a as usize + 1);
                            let step = self.get_register(a as usize + 2);
                            let variable = self.get_register(a as usize + 3);
                            let branch  =
                                self.get_block(instruction_index + sbx as usize - 131070);
                            terminator = Some(
                                UnconditionalJump(
                                    branch)
                                .into(),
                            );
                        }
                        OpCode::ForLoop => {
                            let init = self.get_register(a as usize);
                            let limit = self.get_register(a as usize + 1);
                            let step = self.get_register(a as usize + 2);
                            let variable = self.get_register(a as usize + 3);
                            // TODO: why isnt this just + sbx?
                            let continue_branch =
                                self.get_block(instruction_index + sbx as usize - 131070);
                            let exit_branch = self.get_block(instruction_index + 1);
                            terminator = Some(
                                NumericFor {
                                    variable,
                                    init,
                                    limit,
                                    step,
                                    continue_branch,
                                    exit_branch,
                                }
                                .into(),
                            )
                        }
                        OpCode::Jump => {
                            let branch = self.get_block(instruction_index + sbx as usize - 131070);
                            terminator = Some(UnconditionalJump(branch).into());
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        if !Self::is_terminator(&self.function.code[block_end]) {
            let branch = self.get_block(block_end + 1);
            terminator = Some(UnconditionalJump(branch).into());
        }

        Ok((instructions, terminator))
    }

    pub fn lift_function(&mut self) -> Result<Function<'a>> {
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

        for i in 0..self.function.num_params {
            let parameter = self.lifted_function.new_value();
            self.lifted_function.parameters.push(parameter);
            self.register_map.insert(i as usize, parameter);
        }

        for (block_start_pc, block_end_pc) in block_ranges {
            let cfg_block_id = self.get_block(block_start_pc);
            let (instructions, terminator) =
                self.lift_instructions(block_start_pc, block_end_pc, cfg_block_id)?;

            let block = self.lifted_function.block_mut(cfg_block_id).unwrap();
            block.inner_instructions.extend(instructions);

            self.lifted_function
                .set_block_terminator(cfg_block_id, terminator)?;
        }

        let mut function = self.lifted_function.clone();
        function.set_entry(self.get_block(0))?;

        //function.remove_unconnected_blocks().unwrap();
        Ok(function)
    }
}
