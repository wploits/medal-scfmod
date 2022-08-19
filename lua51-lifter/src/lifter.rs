use std::{borrow::Cow, cell::RefCell, rc::Rc};

use either::Either;
use fxhash::FxHashMap;
use itertools::Itertools;

use ast::RcLocal;
use cfg::{block::Edges, function::Function};
use graph::NodeId;
use lua51_deserializer::{
    argument::{Constant, Register, RegisterOrConstant},
    Function as BytecodeFunction, Instruction, Value,
};

pub struct LifterContext<'a> {
    bytecode: &'a BytecodeFunction<'a>,
    nodes: FxHashMap<usize, NodeId>,
    locals: FxHashMap<Register, RcLocal>,
    constants: FxHashMap<usize, ast::Literal>,
    function: Function,
}

impl<'a> LifterContext<'a> {
    fn allocate_locals(&mut self) {
        for i in 0..self.bytecode.maximum_stack_size {
            let local = self.function.local_allocator.borrow_mut().allocate();

            if i < self.bytecode.number_of_parameters {
                self.function.parameters.push(local.clone());
            }

            self.locals.insert(Register(i), local);
        }
    }

    fn create_block_map(&mut self) {
        self.nodes.insert(0, self.function.new_block());
        for (insn_index, insn) in self.bytecode.code.iter().enumerate() {
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
                    self.nodes
                        .entry(insn_index + 2)
                        .or_insert_with(|| self.function.new_block());
                }
                Instruction::Equal { .. }
                | Instruction::LessThan { .. }
                | Instruction::LessThanOrEqual { .. }
                | Instruction::Test { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                    self.nodes
                        .entry(insn_index + 2)
                        .or_insert_with(|| self.function.new_block());
                }
                Instruction::Jump(step)
                | Instruction::IterateNumericForLoop { step, .. }
                | Instruction::PrepareNumericForLoop { step, .. } => {
                    self.nodes
                        .entry(insn_index + step as usize - 131070)
                        .or_insert_with(|| self.function.new_block());
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                }
                Instruction::Return(..) => {
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                }
                _ => {}
            }
        }
    }

    fn code_ranges(&self) -> Vec<(usize, usize)> {
        let mut nodes = self.nodes.keys().cloned().collect::<Vec<_>>();
        nodes.sort_unstable();
        let ends = nodes
            .iter()
            .skip(1)
            .map(|&s| s - 1)
            .chain(std::iter::once(self.bytecode.code.len() - 1));
        nodes.iter().cloned().zip(ends).collect()
    }

    fn constant(&mut self, constant: Constant) -> ast::Literal {
        let converted_constant = match self.bytecode.constants.get(constant.0 as usize).unwrap() {
            Value::Nil => ast::Literal::Nil,
            Value::Boolean(v) => ast::Literal::Boolean(*v),
            Value::Number(v) => ast::Literal::Number(*v),
            Value::String(v) => ast::Literal::String(v.to_string()),
        };
        self.constants
            .entry(constant.0 as usize)
            .or_insert(converted_constant)
            .clone()
    }

    fn register_or_constant(&mut self, value: RegisterOrConstant) -> ast::RValue {
        match value.0 {
            Either::Left(register) => self.locals[&register].clone().into(),
            Either::Right(constant) => self.constant(constant).into(),
        }
    }

    fn lift_instruction(&mut self, start: usize, end: usize, statements: &mut Vec<ast::Statement>) {
        let mut iter = self.bytecode.code[start..=end].iter();
        while let Some(instruction) = iter.next() {
            match instruction {
                Instruction::Move {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[destination].clone().into()],
                            right: vec![self.locals[source].clone().into()],
                        }
                        .into(),
                    );
                }
                &Instruction::LoadBoolean {
                    destination, value, ..
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[&destination].clone().into()],
                            right: vec![ast::Literal::Boolean(value).into()],
                        }
                        .into(),
                    );
                }
                &Instruction::LoadConstant {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[&destination].clone().into()],
                            right: vec![self.constant(source).into()],
                        }
                        .into(),
                    );
                }
                Instruction::LoadNil(registers) => {
                    for register in registers {
                        statements.push(
                            ast::Assign::new(
                                vec![self.locals[register].clone().into()],
                                vec![ast::Literal::Nil.into()],
                            )
                            .into(),
                        );
                    }
                }
                &Instruction::GetGlobal {
                    destination,
                    global,
                } => {
                    let global_str = self.constant(global).as_string().unwrap().clone();
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Global::new(global_str).into()],
                        )
                        .into(),
                    );
                }
                &Instruction::SetGlobal { destination, value } => {
                    let global_str = self.constant(destination).as_string().unwrap().clone();
                    statements.push(
                        ast::Assign::new(
                            vec![ast::Global::new(global_str).into()],
                            vec![self.locals[&value].clone().into()],
                        )
                        .into(),
                    );
                }
                &Instruction::GetTable {
                    destination,
                    table,
                    key,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Index::new(
                                self.locals[&table].clone().into(),
                                self.register_or_constant(key),
                            )
                            .into()],
                        )
                        .into(),
                    );
                }
                &Instruction::Test {
                    value,
                    comparison_value,
                } => {
                    let value = self.locals[&value].clone().into();
                    let condition = if comparison_value {
                        value
                    } else {
                        ast::Unary::new(value, ast::UnaryOperation::Not).into()
                    };
                    statements.push(ast::If::new(condition, None, None).into())
                }
                Instruction::Not {
                    destination,
                    operand,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Unary::new(
                                self.locals[operand].clone().into(),
                                ast::UnaryOperation::Not,
                            )
                            .into()],
                        )
                        .into(),
                    );
                }
                Instruction::Return(values, _variadic) => {
                    statements.push(
                        ast::Return::new(
                            values
                                .iter()
                                .map(|v| self.locals[v].clone().into())
                                .collect(),
                        )
                        .into(),
                    );
                }
                Instruction::Jump(..) => {}
                &Instruction::Add {
                    destination,
                    lhs,
                    rhs,
                }
                | &Instruction::Sub {
                    destination,
                    lhs,
                    rhs,
                }
                | &Instruction::Mul {
                    destination,
                    lhs,
                    rhs,
                }
                | &Instruction::Div {
                    destination,
                    lhs,
                    rhs,
                }
                | &Instruction::Mod {
                    destination,
                    lhs,
                    rhs,
                }
                | &Instruction::Pow {
                    destination,
                    lhs,
                    rhs,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Binary::new(
                                self.register_or_constant(lhs),
                                self.register_or_constant(rhs),
                                match instruction {
                                    Instruction::Add { .. } => ast::BinaryOperation::Add,
                                    Instruction::Sub { .. } => ast::BinaryOperation::Sub,
                                    Instruction::Mul { .. } => ast::BinaryOperation::Mul,
                                    Instruction::Div { .. } => ast::BinaryOperation::Div,
                                    Instruction::Mod { .. } => ast::BinaryOperation::Mod,
                                    Instruction::Pow { .. } => ast::BinaryOperation::Pow,
                                    _ => unreachable!(),
                                },
                            )
                            .into()],
                        )
                        .into(),
                    );
                }
                Instruction::TestSet {
                    destination,
                    value,
                    comparison_value,
                } => {
                    let value: ast::RValue = self.locals[value].clone().into();
                    let assign = ast::Assign {
                        left: vec![self.locals[destination].clone().into()],
                        right: vec![value.clone()],
                    };
                    let new_block = self.function.new_block();

                    statements.push(
                        ast::If {
                            condition: Box::new(if *comparison_value {
                                ast::Unary {
                                    value: Box::new(value.clone()),
                                    operation: ast::UnaryOperation::Not,
                                }
                                .into()
                            } else {
                                value.clone()
                            }),
                            then_block: None,
                            else_block: None,
                        }
                        .into(),
                    );

                    let condition_block = self.nodes[&start];
                    let next_block = self.nodes[&(end + 1)];
                    let step = match &self.bytecode.code[end] {
                        Instruction::Jump(step) => *step as usize,
                        _ => unreachable!(),
                    };

                    self.function
                        .block_mut(new_block)
                        .unwrap()
                        .ast
                        .push(assign.into());

                    self.function.set_block_terminator(
                        condition_block,
                        Some(Edges::conditional(next_block, new_block)),
                    );

                    self.function.set_block_terminator(
                        new_block,
                        Some(Edges::jump(self.nodes[&(end + step as usize - 131070)])),
                    );
                }
                Instruction::Call {
                    function,
                    arguments,
                    return_values,
                } => {
                    let call = ast::Call {
                        value: Box::new(self.locals[function].clone().into()),
                        arguments: if *arguments <= 1 {
                            Vec::new()
                        } else {
                            (1..*arguments)
                                .map(|argument| {
                                    self.locals[&Register(function.0 + argument)].clone().into()
                                })
                                .collect_vec()
                        },
                    };

                    statements.push(if *return_values > 1 {
                        ast::Assign {
                            left: (0..return_values - 1)
                                .map(|return_value| {
                                    self.locals[&Register(function.0 + return_value)]
                                        .clone()
                                        .into()
                                })
                                .collect_vec(),
                            right: vec![call.into()],
                        }
                        .into()
                    } else {
                        call.into()
                    })
                }
                Instruction::GetUpvalue {
                    destination,
                    upvalue,
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[destination].clone().into()],
                            right: vec![self.function.upvalues_captured[upvalue.0 as usize]
                                .clone()
                                .into()],
                        }
                        .into(),
                    );
                }
                Instruction::Closure {
                    destination,
                    function,
                } => {
                    let closure_bytecode = &self.bytecode.closures[function.0 as usize];
                    let mut upvalues = Vec::new();
                    for _ in 0..closure_bytecode.number_of_upvalues {
                        match iter.next() {
                            Some(Instruction::Move { source, .. }) => {
                                upvalues.push(self.locals[source].clone());
                            }
                            Some(Instruction::GetUpvalue { upvalue, .. }) => {
                                upvalues.push(
                                    self.function.upvalues_captured[upvalue.0 as usize].clone(),
                                );
                            }
                            _ => panic!("unexpected instruction"),
                        }
                    }

                    let closure = Self::lift(
                        closure_bytecode,
                        upvalues.clone(),
                        self.function.local_allocator.clone(),
                    );
                    let parameters = closure.parameters.clone();
                    let body = restructure::lift(closure);

                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[destination].clone().into()],
                            right: vec![ast::Closure {
                                parameters,
                                body,
                                upvalues,
                            }
                            .into()],
                        }
                        .into(),
                    );
                }
                Instruction::Close(start) => statements.push(
                    ast::Close {
                        locals: (start.0..self.bytecode.maximum_stack_size)
                            .map(|i| self.locals[&Register(i)].clone())
                            .collect(),
                    }
                    .into(),
                ),
                _ => statements.push(ast::Comment::new(format!("{:?}", instruction)).into()),
            }

            if matches!(instruction, Instruction::Return { .. }) {
                break;
            }
        }
    }

    fn lift_blocks(&mut self) {
        let ranges = self.code_ranges();
        for (start, end) in ranges {
            let mut block = ast::Block::default();
            self.lift_instruction(start, end, &mut block);
            self.function.block_mut(self.nodes[&start]).unwrap().ast = block;

            match self.bytecode.code[end] {
                Instruction::Equal { .. }
                | Instruction::LessThan { .. }
                | Instruction::LessThanOrEqual { .. }
                | Instruction::Test { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Edges::conditional(
                            self.nodes[&(end + 1)],
                            self.nodes[&(end + 2)],
                        )),
                    );
                }
                Instruction::Jump(step)
                | Instruction::IterateNumericForLoop { step, .. }
                | Instruction::PrepareNumericForLoop { step, .. } => {
                    let block = self.nodes[&start];

                    if self.function.block(block).unwrap().terminator.is_none() {
                        self.function.set_block_terminator(
                            block,
                            Some(Edges::jump(self.nodes[&(end + step as usize - 131070)])),
                        );
                    }
                }
                Instruction::Return { .. } => {}
                Instruction::LoadBoolean { skip_next, .. } => {
                    let successor = self.nodes[&(end + 1 + skip_next as usize)];

                    self.function
                        .set_block_terminator(self.nodes[&start], Some(Edges::jump(successor)));
                }
                _ => {
                    if end + 1 != self.bytecode.code.len() {
                        self.function.set_block_terminator(
                            self.nodes[&start],
                            Some(Edges::jump(self.nodes[&(end + 1)])),
                        );
                    }
                }
            }
        }
    }

    pub fn lift(
        bytecode: &'a BytecodeFunction,
        upvalues: Vec<ast::RcLocal>,
        local_allocator: Rc<RefCell<ast::local_allocator::LocalAllocator>>,
    ) -> Function {
        let mut function = Function::new(local_allocator);
        function.upvalues_captured = upvalues;
        let mut context = Self {
            bytecode,
            nodes: FxHashMap::default(),
            locals: FxHashMap::default(),
            constants: FxHashMap::default(),
            function,
        };

        context.create_block_map();
        context.allocate_locals();
        context.lift_blocks();
        context.function.set_entry(context.nodes[&0]);

        context.function
    }
}
