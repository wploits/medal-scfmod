use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, BTreeMap};
use std::ops::{Range, RangeInclusive, Bound};
use std::rc::Rc;

use anyhow::Result;
use cfg_ir::value_allocator::ValueAllocator;
use graph::NodeId;
use graph::algorithms::dfs_tree;
use graph::algorithms::dominators::{common_dominator, compute_immediate_dominators, dominators};

use super::{
    deserializer::chunk::Chunk,
    deserializer::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, deserializer::constant::Constant as BytecodeConstant,
};

use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{
        location::{ InstructionLocation, InstructionIndex },

        Binary, BinaryOp, ConditionalJump, LoadConstant, Move, UnconditionalJump,
        Unary, UnaryOp, LoadGlobal, StoreGlobal, LoadIndex, Return, Concat, NumericFor,
        Call, Closure, StoreIndex, LoadUpvalue, StoreUpvalue, LoadTable,

        Inner, Terminator, Upvalue
    },
    value::ValueId,
};

pub struct Lifter<'a> {
    function: usize,
    function_list: &'a Vec<BytecodeFunction>,
    string_table: &'a Vec<String>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant<'a>>,
    closures: Vec<Option<Rc<Function<'a>>>>,
    location_map: HashMap<usize, InstructionLocation>,
}

impl<'a> Lifter<'a> {
    pub fn new(
        f_list: &'a Vec<BytecodeFunction>, 
        str_list: &'a Vec<String>, 
        function_id: usize,
        value_allocator: Rc<RefCell<ValueAllocator>>) -> Self {
        Lifter {
            function: function_id,
            function_list: f_list,
            string_table: str_list,
            blocks: HashMap::new(),
            lifted_function: Function::new(value_allocator),
            register_map: HashMap::new(),
            constant_map: HashMap::new(),
            closures: vec![None; f_list[function_id].functions.len()],
            location_map: HashMap::new(),
        }
    }

    fn discover_blocks(&mut self) -> Result<()> {
        self.blocks.insert(0, self.lifted_function.new_block()?);
        for (insn_index, insn) in self.function_list[self.function].instructions.iter().enumerate() {
            match insn {
                BytecodeInstruction::ABC { op_code, c, .. } => match op_code {
                    OpCode::LOP_LOADB if *c != 0 => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*c as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    _ => {}
                }

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_JUMPIF | OpCode::LOP_JUMPIFNOT | 
                    OpCode::LOP_JUMPIFEQ | OpCode::LOP_JUMPIFLE | OpCode::LOP_JUMPIFLT | 
                    OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT |
                    OpCode::LOP_JUMPIFEQK | OpCode::LOP_JUMPIFNOTEQK => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_FORNPREP => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_FORNLOOP => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_FORGPREP |
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
        *self
            .register_map
            .entry(index)
            .or_insert_with(|| self.lifted_function.value_allocator.borrow_mut().new_value())
    }

    fn get_register_range(&mut self, range: Range<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn get_register_incl_range(&mut self, range: RangeInclusive<usize>) -> Vec<ValueId> {
        range.map(|v| self.get_register(v)).collect()
    }

    fn convert_constant(&self, constant: &'a BytecodeConstant) -> Constant<'a> {
        match constant {
            BytecodeConstant::Nil => Constant::Nil,
            BytecodeConstant::Boolean(b) => Constant::Boolean(*b),
            BytecodeConstant::Number(n) => Constant::Number(*n),
            BytecodeConstant::String(s_idx) => Constant::String(Cow::Owned(self.string_table[*s_idx - 1].clone())),
            _ => unimplemented!()
        }
    }

    fn get_constant(&mut self, index: usize) -> Constant<'a> {
        let converted_constant = self.convert_constant(self.function_list[self.function].constants.get(index).unwrap());
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn get_block_constant(&mut self, index: usize, block_index: NodeId) -> (Inner<'a>, ValueId) {
        let constant = self.get_constant(index);
        let value = self.lifted_function.value_allocator.borrow_mut().new_value();
        (
            LoadConstant {
                dest: value,
                constant,
            }
            .into(),
            value
        )
    }

    fn get_block(&self, insn_index: usize) -> NodeId {
        *self.blocks.get(&insn_index).unwrap()
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
                OpCode::LOP_FORNPREP |
                OpCode::LOP_FORNLOOP |
                OpCode::LOP_FORGPREP |
                OpCode::LOP_FORGLOOP | 
                OpCode::LOP_FORGPREP_INEXT | 
                OpCode::LOP_FORGLOOP_INEXT |
                OpCode::LOP_FORGLOOP_NEXT => true,
                _ => false
            },
            BytecodeInstruction::E { op_code, .. } => matches!(op_code, OpCode::LOP_JUMPX),
        }
    }

    fn lift_instructions(
        &mut self, 
        block_start: usize, 
        block_end: usize, 
        mut cfg_block_id: NodeId
    ) -> Result<(Vec<Inner<'a>>, Option<Terminator>)> {
        let mut instructions = Vec::new();
        let mut terminator = None;
        let mut top_index = None;
        let mut iterator = self.function_list[self.function]
            .instructions[block_start..=block_end]
            .iter()
            .enumerate()
            .peekable();

        while let Some((block_instruction_index, instruction)) = iterator.next()
        {
            let instruction_index = block_start + block_instruction_index;
            match *instruction {
                BytecodeInstruction::ABC { op_code, a, b, c, aux } => match op_code {
                    OpCode::LOP_LOADNIL => {
                        let dest = self.get_register(a as usize);
                        instructions.push(LoadConstant { dest, constant: Constant::Nil }.into());
                    }
                    OpCode::LOP_LOADB => {
                        let dest = self.get_register(a as usize);
                        if c != 0 {
                            let branch = self.get_block(instruction_index + c as usize + 1);
                            instructions
                                .push(
                                    LoadConstant {
                                        dest,
                                        constant: Constant::Boolean(b != 0),
                                    }
                                    .into(),
                                );
                            terminator = Some(UnconditionalJump(branch).into());
                            if instruction_index != block_end {
                                cfg_block_id = self.lifted_function.new_block().unwrap();
                            }
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
                    OpCode::LOP_MOVE => {
                        let (dest, source) =
                            (self.get_register(a as usize), self.get_register(b as usize));
                        instructions.push(Move { dest, source }.into());
                    }
                    OpCode::LOP_GETGLOBAL => {
                        let dest = self.get_register(a as usize);
                        let name = self.get_constant(aux as usize);
                        if let Constant::String(name) = name {
                            instructions.push(LoadGlobal { dest, name }.into());
                        }
                    }
                    OpCode::LOP_SETGLOBAL => {
                        let value = self.get_register(a as usize);
                        let name = self.get_constant(aux as usize);
                        if let Constant::String(name) = name {
                            instructions.push(StoreGlobal { name, value }.into());
                        }
                    }
                    OpCode::LOP_GETUPVAL => {
                        let dest = self.get_register(a as usize);
                        instructions.push(
                            LoadUpvalue {
                                dest,
                                upvalue_index: b as usize,
                            }
                            .into(),
                        );
                    }
                    OpCode::LOP_SETUPVAL => {
                        let value = self.get_register(a as usize);
                        instructions.push(
                            StoreUpvalue {
                                upvalue_index: b as usize,
                                value,
                            }
                            .into(),
                        );
                    }
                    OpCode::LOP_CLOSEUPVALS => {},//unimplemented!(),
                    OpCode::LOP_GETTABLE => {
                        let dest = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.get_register(c as usize);

                        instructions.push(LoadIndex { dest, object, key }.into());
                    },
                    OpCode::LOP_SETTABLE => {
                        let value = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.get_register(c as usize);

                        instructions.push(StoreIndex { value, object, key }.into());
                    },
                    OpCode::LOP_GETTABLEKS => {
                        let dest = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let (load, key) = self.get_block_constant(aux as usize, cfg_block_id);

                        instructions.push(load);
                        instructions.push(LoadIndex { dest, object, key }.into());
                    },
                    OpCode::LOP_SETTABLEKS => {
                        let value = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let (load, key) = self.get_block_constant(aux as usize, cfg_block_id);

                        instructions.push(load);
                        instructions.push(StoreIndex { value, object, key }.into());
                    },
                    OpCode::LOP_GETTABLEN => {
                        let dest = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.lifted_function.value_allocator.borrow_mut().new_value();

                        instructions
                            .push(
                                LoadConstant {
                                    dest: key,
                                    constant: Constant::Number(f64::from(c as i16 + 1)),
                                }
                                .into(),
                            );
                        instructions.push(LoadIndex { dest, object, key }.into());
                    },
                    OpCode::LOP_SETTABLEN => {
                        let value = self.get_register(a as usize);
                        let object = self.get_register(b as usize);
                        let key = self.lifted_function.value_allocator.borrow_mut().new_value();

                        instructions
                            .push(
                                LoadConstant {
                                    dest: key,
                                    constant: Constant::Number(f64::from(c as i16 + 1)),
                                }
                                .into(),
                            );
                        instructions.push(StoreIndex { value, object, key }.into());
                    },
                    OpCode::LOP_NAMECALL => unimplemented!(),
                    OpCode::LOP_CALL => {
                        let function = self.get_register(a as usize);
                        let mut arguments = (a as u16 + 1..a as u16 + b as u16)
                            .map(|v| self.get_register(v as usize))
                            .collect::<Vec<_>>();
                        let return_values = (a as u16..a as u16 + c as u16 - 1)
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
                    OpCode::LOP_RETURN => {
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
                    OpCode::LOP_ADD
                    | OpCode::LOP_SUB
                    | OpCode::LOP_MUL
                    | OpCode::LOP_DIV
                    | OpCode::LOP_MOD
                    | OpCode::LOP_POW
                    | OpCode::LOP_AND
                    | OpCode::LOP_OR => {
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
                            OpCode::LOP_AND => BinaryOp::LogicalAnd,
                            OpCode::LOP_OR => BinaryOp::LogicalOr,
                            _ => unreachable!(),
                        };
                        instructions.push(Binary { dest, lhs, rhs, op }.into());
                    }
                    OpCode::LOP_ADDK
                    | OpCode::LOP_SUBK
                    | OpCode::LOP_MULK
                    | OpCode::LOP_DIVK
                    | OpCode::LOP_MODK
                    | OpCode::LOP_POWK
                    | OpCode::LOP_ANDK
                    | OpCode::LOP_ORK  => {
                        let (dest, lhs, (load, rhs)) = (
                            self.get_register(a as usize),
                            self.get_register(b as usize),
                            self.get_block_constant(c as usize, cfg_block_id),
                        );
                        let op = match op_code {
                            OpCode::LOP_ADDK => BinaryOp::Add,
                            OpCode::LOP_SUBK => BinaryOp::Sub, // and breedable
                            OpCode::LOP_MULK => BinaryOp::Mul,
                            OpCode::LOP_DIVK => BinaryOp::Div,
                            OpCode::LOP_MODK => BinaryOp::Mod,
                            OpCode::LOP_POWK => BinaryOp::Pow,
                            OpCode::LOP_ANDK => BinaryOp::LogicalAnd,
                            OpCode::LOP_ORK => BinaryOp::LogicalOr,
                            _ => unreachable!(),
                        };
                        instructions.push(load);
                        instructions.push(Binary { dest, lhs, rhs, op }.into());
                    }
                    OpCode::LOP_CONCAT => {
                        let dest = self.get_register(a as usize);
                        let values = self.get_register_incl_range(b as usize..=c as usize);
                        instructions.push(Concat { dest, values }.into());
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
                        instructions.push(Unary { dest, value, op }.into());
                    }
                    OpCode::LOP_NEWTABLE =>{
                        let dest = self.get_register(a as usize);
                        let mut elems = Vec::new();
                        let mut store_indices = Vec::new();
                        let mut num_records = 0;

                        todo!(); // this is unfinished in the 5.1 lifter and i dont feel like trying to finish its impl rn

                        instructions.push(
                            LoadTable {
                                dest,
                                elems,
                            }.into()
                        );
                        instructions.extend(store_indices.into_iter());
                    }
                    OpCode::LOP_SETLIST => unimplemented!(),
                    OpCode::LOP_GETVARARGS => unimplemented!(),
                    OpCode::LOP_LOADKX => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(aux as usize);
                        instructions.push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_FASTCALL => {}, // we can just ignore these
                    OpCode::LOP_FASTCALL1 => {},
                    OpCode::LOP_FASTCALL2 => {},
                    OpCode::LOP_FASTCALL2K => {},
                    _ => {}
                },

                BytecodeInstruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_LOADN => {
                        let dest = self.get_register(a as usize);
                        let constant = Constant::Number(f64::from(d));
                        instructions.push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_LOADK => {
                        let dest = self.get_register(a as usize);
                        let constant = self.get_constant(d as usize);
                        instructions.push(LoadConstant { dest, constant }.into());
                    }
                    OpCode::LOP_GETIMPORT => {
                        let dest = self.get_register(a as usize);
                        let count = aux >> 30;
                        let id0 = (aux >> 20) & 1023;
                        let id1 = (aux >> 10) & 1023;
                        let id2 = aux & 1023;

                        if count > 2 {
                            if let Constant::String(name1) = self.get_constant(id0 as usize) {
                                let (load2, name2) = self.get_block_constant(id1 as usize, cfg_block_id);
                                let (load3, name3) = self.get_block_constant(id2 as usize, cfg_block_id);
                                instructions.push(LoadGlobal { dest, name: name1 }.into());
                                instructions.push(load2);
                                instructions.push(LoadIndex { dest, object: dest, key: name2 }.into());
                                instructions.push(load3);
                                instructions.push(LoadIndex { dest, object: dest, key: name3 }.into());
                            }
                        }
                        else if count > 1 {
                            if let Constant::String(name1) = self.get_constant(id0 as usize) {
                                let (load2, name2) = self.get_block_constant(id1 as usize, cfg_block_id);
                                instructions.push(LoadGlobal { dest, name: name1 }.into());
                                instructions.push(load2);
                                instructions.push(LoadIndex { dest, object: dest, key: name2 }.into());
                            }
                        }
                        else {
                            if let Constant::String(name) = self.get_constant(id0 as usize) {
                                instructions.push(LoadGlobal { dest, name }.into());
                            }
                        }
                    }
                    OpCode::LOP_NEWCLOSURE | OpCode::LOP_DUPCLOSURE => {
                        let dest = self.get_register(a as usize);
                        let child_id = match op_code {
                            OpCode::LOP_NEWCLOSURE => self.function_list[self.function].functions[d as usize],
                            OpCode::LOP_DUPCLOSURE => match self.function_list[self.function].constants.get(d as usize).unwrap() {
                                BytecodeConstant::Closure(f_id) => self.function_list[self.function].functions[*f_id],
                                _ => unreachable!()
                            }
                            _ => unreachable!()
                        };

                        let mut upvalues = Vec::with_capacity(self.function_list[child_id].num_upvalues as usize);
                        for _ in 0..upvalues.capacity() {
                            match *iterator.next().unwrap().1 {
                                BytecodeInstruction::ABC { op_code: OpCode::LOP_CAPTURE, a, b, .. } => match a {
                                    0 | 1 => {
                                        upvalues.push(Upvalue::Value(self.get_register(b as usize)))
                                    }
                                    2 => {
                                        upvalues.push(Upvalue::Upvalue(b as usize))
                                    }
                                    _ => unreachable!()
                                }
                                _ => unreachable!(),
                            }
                        }
                        let lifted_child = if let Some(lifted_child) = &self.closures[d as usize] {
                            lifted_child.clone()
                        } else {
                            let lifted_child = Lifter::new(
                                &self.function_list,
                                &self.string_table,
                                child_id,
                                self.lifted_function.value_allocator.clone())
                                .lift_function()
                                .map(Rc::new)?;
                            self.closures[d as usize] = Some(lifted_child.clone());
                            lifted_child
                        };

                        instructions.push(Inner::Closure(Closure {
                            dest,
                            function: lifted_child,
                            upvalues,
                        }));
                    }
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        let branch = self
                            .get_block(instruction_index.wrapping_add(d as usize) + 1);
                        terminator = Some(UnconditionalJump(branch).into());
                    }
                    OpCode::LOP_JUMPIF | OpCode::LOP_JUMPIFNOT => {
                        let condition = self.get_register(a as usize);
                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1),
                            self.get_block(instruction_index + 1),
                        );
                        if matches!(op_code, OpCode::LOP_JUMPIFNOT) {
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
                    OpCode::LOP_JUMPIFEQ | OpCode::LOP_JUMPIFLE | OpCode::LOP_JUMPIFLT |
                    OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT => {
                        let (lhs, rhs) = (
                            self.get_register(a as usize),
                            self.get_register(aux as usize),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1),
                            self.get_block(instruction_index + 1),
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
                        let condition = self.lifted_function.value_allocator.borrow_mut().new_value();
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
                    OpCode::LOP_DUPTABLE => {
                        // notes for the future:
                        // this instruction is only invoked when there is no array part to the table and its not empty
                        unimplemented!();
                    }
                    OpCode::LOP_FORNPREP => {
                        let branch = self.get_block(instruction_index.wrapping_add(d as usize) + 1);
                        terminator = Some(UnconditionalJump(branch).into());
                    }
                    OpCode::LOP_FORNLOOP => {
                        let limit = self.get_register(a as usize);
                        let step = self.get_register(a as usize + 1);
                        let init = self.get_register(a as usize + 2);

                        let (init_reg, vari_reg) = (a as usize + 2, a as usize + 3);
                        let variable = 
                            match self.function_list[self.function].instructions[instruction_index.wrapping_add(d as usize) + 1] {
                                BytecodeInstruction::ABC { op_code: OpCode::LOP_MOVE, a, b, .. } 
                                    if a as usize == vari_reg && b as usize == init_reg => self.get_register(a as usize + 3),
                                _ => self.get_register(a as usize + 2)
                            };
                        
                        let continue_branch = self
                            .get_block(instruction_index.wrapping_add(d as usize) + 1);
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
                    OpCode::LOP_FORGLOOP => unimplemented!(),
                    OpCode::LOP_FORGPREP_INEXT => unimplemented!(),
                    OpCode::LOP_FORGLOOP_INEXT => unimplemented!(),
                    OpCode::LOP_FORGPREP_NEXT => unimplemented!(),
                    OpCode::LOP_FORGLOOP_NEXT => unimplemented!(),
                    //OpCode::LOP_DUPCLOSURE => unimplemented!(),
                    OpCode::LOP_JUMPIFEQK | OpCode::LOP_JUMPIFNOTEQK => {
                        let (lhs, (load, rhs)) = (
                            self.get_register(a as usize),
                            self.get_block_constant(aux as usize, cfg_block_id),
                        );

                        let (mut true_branch, mut false_branch) = (
                            self.get_block(instruction_index.wrapping_add(d as usize) + 1),
                            self.get_block(instruction_index + 1),
                        );

                        if matches!(op_code, OpCode::LOP_JUMPIFNOTEQ | OpCode::LOP_JUMPIFNOTLE | OpCode::LOP_JUMPIFNOTLT) {
                            std::mem::swap(&mut true_branch, &mut false_branch);
                        }

                        let op = BinaryOp::Equal;
                        let condition = self.lifted_function.value_allocator.borrow_mut().new_value();
                        instructions.push(load);
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
                    OpCode::LOP_FORGPREP => unimplemented!(),
                    _ => {}
                },

                BytecodeInstruction::E { op_code, e } => match op_code {
                    OpCode::LOP_JUMPX => {
                        unimplemented!();
                    }
                    _ => {}
                }
            }
            if !instructions.is_empty() {
                self.location_map.insert(instruction_index, InstructionLocation { node: cfg_block_id, index: InstructionIndex::Inner(instructions.len() - 1) });
            }
        }

        if !Self::is_terminator(&self.function_list[self.function].instructions[block_end]) {
            let branch = self.get_block(block_end + 1);
            terminator = Some(UnconditionalJump(branch).into());
        }

        Ok((instructions, terminator))
    }

    pub fn lift_function(mut self) -> Result<Function<'a>> {
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

        for i in 0..self.function_list[self.function].num_parameters {
            let parameter = self.lifted_function.value_allocator.borrow_mut().new_value();
            self.lifted_function.parameters.push(parameter);
            self.register_map.insert(i as usize, parameter);
        }

        for &(block_start_pc, block_end_pc) in &block_ranges {
            let cfg_block_id = self.get_block(block_start_pc);
            let (instructions, terminator) =
                self.lift_instructions(block_start_pc, block_end_pc, cfg_block_id)?;

            let block = self.lifted_function.block_mut(cfg_block_id).unwrap();
            block.inner_instructions.extend(instructions);

            self.lifted_function
                .set_block_terminator(cfg_block_id, terminator)?;
        }

        self.lifted_function.set_entry(self.get_block(0))?;

        let dfs = dfs_tree(self.lifted_function.graph(), self.lifted_function.entry().unwrap())?;
        let idoms = compute_immediate_dominators(self.lifted_function.graph(), self.lifted_function.entry().unwrap(), &dfs)?;
        let dominators = dominators(self.lifted_function.graph(), self.lifted_function.entry().unwrap(), &idoms).unwrap();
        let mut open_values = BTreeMap::new();
        for (block_start, block_end) in block_ranges.into_iter().rev() {
            let mut it = self.function_list[self.function]
                .instructions[block_start..=block_end]
                .iter()
                .enumerate();
            while let Some((block_instruction_index, instruction)) = it.next() {
                let instruction_index = block_start + block_instruction_index;
                match *instruction {
                    BytecodeInstruction::AD { op_code, d, .. } => match op_code {
                        OpCode::LOP_NEWCLOSURE | OpCode::LOP_DUPCLOSURE => {
                            let child_id = match op_code {
                                OpCode::LOP_NEWCLOSURE => self.function_list[self.function].functions[d as usize],
                                OpCode::LOP_DUPCLOSURE => match self.function_list[self.function].constants.get(d as usize).unwrap() {
                                    BytecodeConstant::Closure(f_id) => self.function_list[self.function].functions[*f_id],
                                    _ => unreachable!()
                                }
                                _ => unreachable!()
                            };
                            let num_upvalues = self.function_list[child_id].num_upvalues;

                            for _ in 0..num_upvalues {
                                match *it.next().unwrap().1 {
                                    BytecodeInstruction::ABC { op_code: OpCode::LOP_CAPTURE, a, b, .. } => match a {
                                        0 | 1 => {
                                            open_values.entry(b as usize).or_insert_with(Vec::new).push(self.location_map[&instruction_index]);
                                        }
                                        2 => {

                                        }
                                        _ => unreachable!()
                                    },
                                    _ => unreachable!(),
                                }
                            }
                        }
                        _ => {}
                    },
                    BytecodeInstruction::ABC { op_code: OpCode::LOP_CLOSEUPVALS, a, .. } => {
                        let values = open_values.range((Bound::Unbounded, Bound::Included(a as usize))).map(|(&v, _)| v).collect::<Vec<_>>();
                        for value in values {
                            let open_locations = open_values.remove(&value).unwrap();
                            let open_location = if open_locations.len() == 1 {
                                open_locations[0]
                            } else {
                                // TODO: make this take an iter
                                let node = common_dominator(&dominators, open_locations.iter().map(|l| l.node).collect::<Vec<_>>()).unwrap();
                                InstructionLocation { node, index: InstructionIndex::Terminator }
                            };
                            let close_location = self.location_map[&instruction_index];

                            let reg = self.get_register(value);
                            self.lifted_function.upvalue_open_ranges.entry(reg).or_insert_with(Vec::new).push((open_location, close_location));
                        }
                    },
                    _ => {}
                }
            }
        }

        Ok(self.lifted_function)
    }
}
