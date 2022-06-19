use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    ops::{Bound, Range, RangeInclusive},
    rc::Rc,
};

use anyhow::Result;

use cfg_ir::{
    constant::Constant,
    function::Function,
    instruction::{
        location::{InstructionIndex, InstructionLocation},
        Binary, BinaryOp, Call, Closure, Concat, ConditionalJump, Inner, LoadConstant, LoadGlobal,
        LoadIndex, LoadTable, LoadUpvalue, Move, NumericFor, Return, StoreGlobal, StoreIndex,
        StoreUpvalue, Terminator, Unary, UnaryOp, UnconditionalJump, Upvalue,
    },
    value::ValueId,
    value_allocator::ValueAllocator,
};
use graph::{
    algorithms::{
        dfs_tree,
        dominators::{common_dominator, compute_immediate_dominators, dominators},
    },
    NodeId,
};

use crate::instruction::Instruction;

use super::{
    chunk::function::Function as BytecodeFunction, instruction::Instruction as BytecodeInstruction,
    op_code::OpCode, value::ValueId as BytecodeValueId,
};

pub struct Lifter<'a> {
    function: &'a BytecodeFunction<'a>,
    blocks: HashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    location_map: HashMap<usize, InstructionLocation>,
    closures: Vec<Option<Rc<Function<'a>>>>,
    register_map: HashMap<usize, ValueId>,
    constant_map: HashMap<usize, Constant<'a>>,
}

impl<'a> Lifter<'a> {
    pub fn new(
        function: &'a BytecodeFunction<'_>,
        value_allocator: Rc<RefCell<ValueAllocator>>,
    ) -> Self {
        Self {
            function,
            blocks: HashMap::new(),
            lifted_function: Function::new(value_allocator),
            location_map: HashMap::new(),
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
            let value = self
                .lifted_function
                .value_allocator
                .borrow_mut()
                .new_value();
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
                        | OpCode::TableForLoop
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
        let mut self_map = HashMap::new();
        let mut iterator = self.function.code[block_start..=block_end]
            .iter()
            .enumerate()
            .peekable();

        while let Some((block_instruction_index, instruction)) = iterator.next() {
            let old_instructions_len = instructions.len();
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
                    OpCode::GetUpvalue => {
                        let dest = self.get_register(a as usize);
                        instructions.push(
                            LoadUpvalue {
                                dest,
                                upvalue_index: b as usize,
                            }
                            .into(),
                        );
                    }
                    OpCode::SetUpvalue => {
                        let value = self.get_register(a as usize);
                        instructions.push(
                            StoreUpvalue {
                                upvalue_index: b as usize,
                                value,
                            }
                            .into(),
                        );
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
                    OpCode::NewTable => {
                        let dest = self.get_register(a as usize);
                        let mut elems = Vec::new();
                        let mut store_indices = Vec::new();
                        let mut num_records = 0;

                        while let Some((_, instruction)) = iterator.peek() {
                            match (instruction.opcode(), instruction) {
                                (OpCode::LoadConst, Instruction::ABx { a, bx, .. }) => {
                                    let a = *a as usize;
                                    let dest = self.get_register(a);
                                    let constant = self.get_constant(*bx as usize);

                                    instructions.push(LoadConstant { dest, constant }.into());
                                    elems.push(ValueId(a + num_records));
                                }
                                (OpCode::NewIndex, Instruction::ABC { a, b, c, .. }) => {
                                    let a = *a as usize;
                                    let b = *b as usize;
                                    let object = self.get_register(a);
                                    let key = self.get_register_or_constant(b, cfg_block_id);
                                    let value =
                                        self.get_register_or_constant(*c as usize, cfg_block_id);

                                    store_indices.push(StoreIndex { value, object, key }.into());
                                    num_records += 2;
                                }
                                (OpCode::SetList, _) => {}
                                _ => break,
                            }

                            iterator.next();
                        }

                        instructions.push(LoadTable { dest, elems }.into());
                        instructions.extend(store_indices.into_iter());
                    }
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

                        let condition = self
                            .lifted_function
                            .value_allocator
                            .borrow_mut()
                            .new_value();
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
                        let (function, table) = match self_map.get(&function) {
                            Some(method) => (*method, Some(function)),
                            None => (function, None)
                        };

                        if b == 0 {
                            assert!(top_index.is_some());
                            arguments = self.get_register_range(a as usize + 1..top_index.unwrap());
                        }
                        if c == 0 {
                            top_index = Some(a as usize);
                        }

                        instructions.push(Call {
                            function,
                            arguments,
                            variadic_arguments: b == 0,
                            return_values,
                            variadic_return: c == 0,
                            table,
                        }.into());
                    }
                    OpCode::Self_ => {
                        let function = self.get_register(a as usize);
                        let method = self.get_register_or_constant(c as usize, cfg_block_id);

                        self_map.insert(function, method);
                    }
                    /*OpCode::TableForLoop => {
                        let iterator = self.get_register(a as usize);
                        let values = self.get_register_incl_range(a as usize..=a as usize + c as usize);
                        terminator = Some(
                            TableForLoop {

                            }
                        );
                    }*/
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
                    OpCode::Close => {}
                    OpCode::SetList => {
                        // TODO: setlist
                        if c == 0 {
                            iterator.next();
                        }
                    }
                    /*OpCode::VarArg => {
                        vararg_index = Some(a as usize);
                    }*/
                    op => todo!("opcode {:?}", op),
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

                        let child = &self.function.closures[bx as usize];

                        let mut upvalues = Vec::with_capacity(child.num_upvalues as usize);
                        for _ in 0..child.num_upvalues {
                            match *iterator.next().unwrap().1 {
                                BytecodeInstruction::ABC {
                                    op_code: OpCode::Move,
                                    b,
                                    ..
                                } => upvalues.push(Upvalue::Value(self.get_register(b as usize))),
                                BytecodeInstruction::ABC {
                                    op_code: OpCode::GetUpvalue,
                                    b,
                                    ..
                                } => {
                                    // TODO: upvalues[b] might be invalid in constructed bytecode
                                    // TODO: make upvalue enum
                                    upvalues.push(Upvalue::Upvalue(b as usize))
                                }
                                _ => unreachable!(),
                            }
                        }

                        let lifted_child = if let Some(lifted_child) = &self.closures[bx as usize] {
                            lifted_child.clone()
                        } else {
                            let lifted_child =
                                Lifter::new(child, self.lifted_function.value_allocator.clone())
                                    .lift_function()
                                    .map(Rc::new)?;
                            self.closures[bx as usize] = Some(lifted_child.clone());
                            lifted_child
                        };

                        instructions.push(Inner::Closure(Closure {
                            dest,
                            function: lifted_child,
                            upvalues,
                        }));
                    }
                    _ => unreachable!(),
                },

                // TODO: BytecodeInstruction::AsBx { OpCode::Jump, a, sbx, }, etc.
                // so can be in same order as OpCode enum :)
                BytecodeInstruction::AsBx { op_code, a, sbx } => {
                    match op_code {
                        OpCode::ForPrep => {
                            let branch = self.get_block(instruction_index + sbx as usize - 131070);
                            terminator = Some(UnconditionalJump(branch).into());
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
            if !instructions.is_empty() {
                self.location_map.insert(
                    instruction_index,
                    InstructionLocation {
                        node: cfg_block_id,
                        index: InstructionIndex::Inner(old_instructions_len),
                    },
                );
            }
        }

        if !Self::is_terminator(&self.function.code[block_end]) {
            let branch = self.get_block(block_end + 1);
            terminator = Some(UnconditionalJump(branch).into());
        }

        if terminator.is_some() {
            self.location_map.insert(
                block_end,
                InstructionLocation {
                    node: cfg_block_id,
                    index: InstructionIndex::Terminator,
                },
            );
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
            let parameter = self
                .lifted_function
                .value_allocator
                .borrow_mut()
                .new_value();
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
        )
        .unwrap();
        let mut open_values = BTreeMap::new();
        for (block_start, block_end) in block_ranges.into_iter().rev() {
            let mut it = self.function.code[block_start..=block_end]
                .iter()
                .enumerate();
            while let Some((block_instruction_index, instruction)) = it.next() {
                let instruction_index = block_start + block_instruction_index;
                match *instruction {
                    BytecodeInstruction::ABx {
                        op_code: OpCode::Closure,
                        bx,
                        ..
                    } => {
                        let child = &self.function.closures[bx as usize];

                        for _ in 0..child.num_upvalues {
                            match *it.next().unwrap().1 {
                                BytecodeInstruction::ABC {
                                    op_code: OpCode::Move,
                                    b,
                                    ..
                                } => {
                                    open_values
                                        .entry(b as usize)
                                        .or_insert_with(Vec::new)
                                        .push(self.location_map[&instruction_index]);
                                }
                                BytecodeInstruction::ABC {
                                    op_code: OpCode::GetUpvalue,
                                    ..
                                } => {}
                                _ => unreachable!(),
                            }
                        }
                    }
                    BytecodeInstruction::ABC {
                        op_code: OpCode::Return,
                        ..
                    } => {
                        let values = open_values.iter().map(|(&v, _)| v).collect::<Vec<_>>();
                        for value in values {
                            let open_locations = open_values.remove(&value).unwrap();
                            let open_location = if open_locations.len() == 1 {
                                open_locations[0]
                            } else {
                                // TODO: make this take an iter
                                let node = common_dominator(
                                    &dominators,
                                    open_locations.iter().map(|l| l.node).collect::<Vec<_>>(),
                                )
                                .unwrap();
                                InstructionLocation {
                                    node,
                                    index: InstructionIndex::Terminator,
                                }
                            };
                            let close_location = self.location_map[&instruction_index];

                            let reg = self.get_register(value);
                            self.lifted_function
                                .upvalue_open_ranges
                                .entry(reg)
                                .or_insert_with(Vec::new)
                                .push((open_location, close_location));
                        }
                    }
                    BytecodeInstruction::ABC {
                        op_code: OpCode::Close,
                        a,
                        ..
                    } => {
                        let values = open_values
                            .range((Bound::Unbounded, Bound::Included(a as usize)))
                            .map(|(&v, _)| v)
                            .collect::<Vec<_>>();
                        for value in values {
                            let open_locations = open_values.remove(&value).unwrap();
                            let open_location = if open_locations.len() == 1 {
                                open_locations[0]
                            } else {
                                // TODO: make this take an iter
                                let node = common_dominator(
                                    &dominators,
                                    open_locations.iter().map(|l| l.node).collect::<Vec<_>>(),
                                )
                                .unwrap();
                                InstructionLocation {
                                    node,
                                    index: InstructionIndex::Terminator,
                                }
                            };
                            println!("{} {}", self.function.line_defined, self.function.positions.as_ref().unwrap()[instruction_index].source);
                            let close_location = self.location_map[&instruction_index];

                            let reg = self.get_register(value);
                            self.lifted_function
                                .upvalue_open_ranges
                                .entry(reg)
                                .or_insert_with(Vec::new)
                                .push((open_location, close_location));
                        }
                    }
                    BytecodeInstruction::ABC {
                        op_code: OpCode::SetList,
                        c,
                        ..
                    } if c == 0 => {
                        it.next();
                    }
                    _ => {}
                }
            }
        }

        Ok(self.lifted_function)
    }
}
