use std::collections::{HashMap, VecDeque};
use std::{borrow::Cow, cell::RefCell, rc::Rc};

use anyhow::Result;
use either::Either;
use fxhash::FxHashMap;

use ast;
use cfg::{block::Terminator, function::Function};
use graph::NodeId;
use lua51_deserializer::argument::{Constant, Register};
use lua51_deserializer::{
    argument::RegisterOrConstant, Function as BytecodeFunction, Instruction, Value,
};

pub struct Lifter<'a> {
    bytecode_function: &'a BytecodeFunction<'a>,
    blocks: FxHashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    // TODO: make this a ref
    lifted_descendants: Vec<Rc<RefCell<Function<'a>>>>,
    closures: Vec<Option<Rc<RefCell<Function<'a>>>>>,
    register_map: FxHashMap<Register, Rc<ast::Local<'a>>>,
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

        for r in 0..self.bytecode_function.number_of_parameters {
            let parameter = self.lifted_function.local_allocator.allocate();
            self.lifted_function.parameters.push(parameter.clone());
            self.register_map.insert(Register(r), parameter);
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
                Instruction::SetList {
                    block_number: 0, ..
                } => {
                    // TODO: skip next instruction
                    todo!();
                }
                Instruction::LoadBoolean {
                    skip_next: true, ..
                } => {
                    self.blocks
                        .entry(insn_index + 2)
                        .or_insert_with(|| self.lifted_function.new_block().unwrap());
                }
                Instruction::Equal { .. }
                | Instruction::LessThan { .. }
                | Instruction::LessThanOrEqual { .. }
                | Instruction::Test { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.blocks
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    self.blocks
                        .entry(insn_index + 2)
                        .or_insert_with(|| self.lifted_function.new_block().unwrap());
                }
                Instruction::Jump(step)
                | Instruction::IterateNumericForLoop { step, .. }
                | Instruction::PrepareNumericForLoop { step, .. } => {
                    self.blocks
                        .entry(insn_index + step as usize - 131070)
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
        let mut statements = Vec::new();
        let mut table_queue = Vec::new();
        let mut table_elements = Vec::new();
        let mut terminator = None;

        //todo: dont clone instructions
        for (index, instruction) in self.bytecode_function.code[block_start..=block_end]
            .iter()
            .cloned()
            .enumerate()
        {
            let instruction_index = block_start + index;
            match instruction {
                Instruction::Move {
                    destination,
                    source,
                } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![self.register(source).into()],
                        }
                        .into(),
                    );
                }
                Instruction::LoadConstant {
                    destination,
                    source,
                } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![ast::RValue::Literal(self.constant(source))],
                        }
                        .into(),
                    );
                }
                Instruction::LoadBoolean {
                    destination,
                    value,
                    skip_next,
                } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![ast::Literal::Boolean(value).into()],
                        }
                        .into(),
                    );

                    if skip_next {
                        terminator = Some(
                            Terminator::Jump(self.block_to_node(instruction_index + 2)).into(),
                        );
                    }
                }
                Instruction::LoadNil(range) => {
                    let left = range
                        .iter()
                        .map(|&r| ast::LValue::Local(self.register(r)))
                        .collect();
                    let right = vec![ast::Literal::Nil.into(); range.len()];

                    statements.push(ast::Assign { left, right }.into())
                }
                Instruction::GetGlobal {
                    destination,
                    global,
                    ..
                } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![self.global(global).into()],
                        }
                        .into(),
                    );
                }
                Instruction::GetTable {
                    destination,
                    table,
                    key,
                } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    let key = Box::new(self.register_or_constant(key, &mut statements).into());

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![ast::Index {
                                left: Box::new(self.register(table).into()),
                                right: key,
                            }
                            .into()],
                        }
                        .into(),
                    );
                }
                Instruction::SetGlobal { destination, value } => {
                    statements.push(
                        ast::Assign {
                            left: vec![match self.constant(destination).into() {
                                ast::Literal::String(global) => Rc::new(ast::Local(global)).into(),
                                _ => panic!("Invalid global"),
                            }],
                            right: vec![self.register(value).into()],
                        }
                        .into(),
                    );
                }
                Instruction::NewTable { destination, .. } => {
                    if !table_queue.is_empty() {
                        table_elements.push(destination);
                    }

                    table_queue.push(destination);
                }
                Instruction::SetList {
                    table,
                    number_of_elements,
                    ..
                } => match table_queue.pop() {
                    Some(t) if t.0 == table.0 => {
                        statements.push(
                            ast::Assign {
                                left: vec![self.register(table).into()],
                                right: vec![ast::Table(
                                    table_elements
                                        .drain(table_elements.len() - number_of_elements as usize..)
                                        .map(|element| (None, self.register(element).into()))
                                        .collect(),
                                )
                                .into()],
                            }
                            .into(),
                        );
                    }
                    _ => panic!("Invalid table queue"),
                },
                Instruction::Test {
                    value,
                    comparison_value,
                } => {
                    let condition = self.register_or_constant(
                        RegisterOrConstant(Either::Left(value)),
                        &mut statements,
                    );
                    let (mut true_branch, mut false_branch) = (
                        self.block_to_node(instruction_index + 2),
                        self.block_to_node(instruction_index + 1),
                    );
                    if comparison_value {
                        std::mem::swap(&mut true_branch, &mut false_branch);
                    }
                    statements.push(ast::If::new(condition, None, None).into());
                    terminator = Some(Terminator::Conditional(false_branch, true_branch));
                }
                Instruction::Jump(sbx) => {
                    terminator = Some(Terminator::Jump(
                        self.block_to_node(instruction_index + sbx as usize - 131070),
                    ));
                }
                Instruction::Return(registers, is_variadic) => {
                    assert!(!is_variadic);
                    statements.push(
                        ast::Return::new(
                            registers
                                .iter()
                                .map(|&r| ast::RValue::Local(self.register(r)))
                                .collect(),
                        )
                        .into(),
                    );
                    terminator = Some(Terminator::Return);
                    break;
                }
                other => unimplemented!("{:?}", other),
            }
        }
        (
            statements,
            terminator.unwrap_or_else(|| Terminator::Jump(self.block_to_node(block_end + 1))),
        )
    }

    fn register_or_constant(
        &mut self,
        index: RegisterOrConstant,
        statements: &mut Vec<ast::Statement<'a>>,
    ) -> ast::RValue<'a> {
        match index.0 {
            Either::Left(register) => self.register(register).into(),
            Either::Right(constant) => {
                let literal = self.constant(constant);
                let local = self.lifted_function.local_allocator.allocate();
                statements.push(
                    ast::Assign::new(vec![local.clone().into()], vec![literal.into()]).into(),
                );
                local.into()
            }
        }
    }

    fn register(&mut self, register: Register) -> Rc<ast::Local<'a>> {
        self.register_map
            .entry(register)
            .or_insert_with(|| self.lifted_function.local_allocator.allocate())
            .clone()
    }

    fn global(&mut self, constant: Constant) -> ast::Global<'a> {
        match self
            .bytecode_function
            .constants
            .get(constant.0 as usize)
            .unwrap()
        {
            Value::String(string) => ast::Global::new(Cow::Borrowed(string)),
            _ => panic!("invalid global"),
        }
    }

    fn constant(&mut self, constant: Constant) -> ast::Literal<'a> {
        let converted_constant = match self
            .bytecode_function
            .constants
            .get(constant.0 as usize)
            .unwrap()
        {
            Value::Nil => ast::Literal::Nil,
            Value::Boolean(v) => ast::Literal::Boolean(*v),
            Value::Number(v) => ast::Literal::Number(*v),
            Value::String(v) => ast::Literal::String(Cow::Borrowed(v)),
        };
        self.constant_map
            .entry(constant.0 as usize)
            .or_insert(converted_constant)
            .clone()
    }

    fn block_to_node(&self, insn_index: usize) -> NodeId {
        *self.blocks.get(&insn_index).unwrap()
    }
}
