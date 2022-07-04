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
        const FIELDS_PER_FLUSH: u8 = 50;

        let mut statements = Vec::new();
        // Map of tables to their elements.
        let mut tables: HashMap<Register, Vec<ast::RValue>> = HashMap::new();
        let mut table_queue = Vec::new();
        let mut table_block = 0;
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
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![self.register(source).into()],
                        }
                        .into(),
                    );

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }
                }
                Instruction::LoadConstant {
                    destination,
                    source,
                } => {
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![ast::RValue::Literal(self.constant(source))],
                        }
                        .into(),
                    );

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }
                }
                Instruction::LoadBoolean {
                    destination,
                    value,
                    skip_next,
                } => {
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);

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

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }
                }
                Instruction::LoadNil(range) => {
                    let range: Vec<_> = range
                        .into_iter()
                        .map(|r| Register(r.0 + table_block * FIELDS_PER_FLUSH))
                        .collect();
                    let left = range
                        .iter()
                        .map(|&r| ast::LValue::Local(self.register(r)))
                        .collect();
                    let right = vec![ast::Literal::Nil.into(); range.len()];

                    statements.push(
                        ast::Assign {
                            left,
                            right: right.clone(),
                        }
                        .into(),
                    );

                    if let Some(table) = table_queue.last() {
                        tables.get_mut(table).unwrap().extend(right);
                    }
                }
                Instruction::GetGlobal {
                    destination,
                    global,
                    ..
                } => {
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);

                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![self.global(global).into()],
                        }
                        .into(),
                    );

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }
                }
                Instruction::GetTable {
                    destination,
                    table,
                    key,
                } => {
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);
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

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }
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
                    let destination = Register(destination.0 + table_block * FIELDS_PER_FLUSH);

                    tables.insert(destination, Vec::new());

                    if let Some(table) = table_queue.last() {
                        tables
                            .get_mut(table)
                            .unwrap()
                            .push(ast::RValue::Local(self.register(destination)));
                    }

                    table_queue.push(destination);
                }
                Instruction::SetList {
                    table,
                    block_number,
                    ..
                } => match table_queue.last() {
                    Some(current_table) if current_table.0 == table.0 => {
                        table_block = block_number;

                        if self.bytecode_function.code[instruction_index + 1..=block_end]
                            .iter()
                            .find(|instruction| match instruction {
                                Instruction::SetList {
                                    table,
                                    block_number,
                                    ..
                                } if table.0 == current_table.0
                                    && *block_number == table_block + 1 =>
                                {
                                    true
                                }
                                _ => false,
                            })
                            .is_some()
                        {
                            continue;
                        } else {
                            table_block = 0;
                            statements.push(
                                ast::Assign {
                                    left: vec![self.register(table).into()],
                                    right: vec![ast::Table(
                                        tables
                                            .remove(&current_table)
                                            .unwrap()
                                            .into_iter()
                                            .map(|v| (None, v))
                                            .collect(),
                                    )
                                    .into()],
                                }
                                .into(),
                            );
                            table_queue.pop();
                        }
                    }
                    other => panic!(
                        "Invalid table queue: expected {:?}, got {:?}.",
                        table, other
                    ),
                },
                Instruction::Call {
                    function,
                    arguments,
                    return_values,
                } => {
                    let arguments = if arguments >= 2 {
                        (function.0 + 1..function.0 + arguments)
                            .map(|r| self.register(Register(r)).into())
                            .collect()
                    } else {
                        Vec::new()
                    };
                    let number_of_arguments = arguments.len();
                    let variadic = return_values == 0;
                    let return_values = if return_values >= 2 {
                        (function.0..function.0 + return_values - 1)
                            .map(|r| self.register(Register(r)).into())
                            .collect()
                    } else {
                        Vec::new()
                    };

                    if !variadic {
                        statements.push(
                            ast::Assign {
                                left: return_values,
                                right: vec![ast::Call {
                                    value: Box::new(self.register(function).into()),
                                    arguments,
                                }
                                .into()],
                            }
                            .into(),
                        );
                    }

                    if let Some(table) = table_queue.last() {
                        let elements = tables.get_mut(table).unwrap();
                        let arguments = elements
                            .drain(elements.len() - number_of_arguments..)
                            .collect();

                        elements.pop();
                        elements.push(if variadic {
                            ast::Call {
                                value: Box::new(self.register(function).into()),
                                arguments,
                            }
                            .into()
                        } else {
                            self.register(function).into()
                        });
                    }
                }
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
