use std::{io::Read, rc::Rc};

use either::Either;
use fxhash::FxHashMap;
use itertools::Itertools;

use ast::{RcLocal, Statement};
use cfg::{block::Terminator, function::Function};

use lua51_deserializer::{
    argument::{Constant, Register, RegisterOrConstant},
    Function as BytecodeFunction, Instruction, Value,
};

use petgraph::{algo::dominators::simple_fast, stable_graph::NodeIndex};

pub struct LifterContext<'a> {
    bytecode: &'a BytecodeFunction<'a>,
    nodes: FxHashMap<usize, NodeIndex>,
    blocks_to_skip: FxHashMap<usize, usize>,
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
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
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
                Instruction::Jump(skip) => {
                    let dest_index = insn_index + skip as usize - 131070;
                    let dest_block = *self
                        .nodes
                        .entry(dest_index)
                        .or_insert_with(|| self.function.new_block());
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                    // if insn_index != dest_index {
                    //     if let Some(jmp_block) = self.nodes.remove(&insn_index) {
                    //         self.function.remove_block(jmp_block);
                    //         self.nodes.insert(insn_index, dest_block);
                    //         self.blocks_to_skip.insert(insn_index, dest_index);
                    //     }
                    // }
                }
                Instruction::IterateNumericForLoop { skip, .. }
                | Instruction::PrepareNumericForLoop { skip, .. } => {
                    self.nodes
                        .entry(insn_index + skip as usize - 131070)
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
        nodes
            .iter()
            .cloned()
            .zip(ends)
            .filter(|(s, _)| !self.blocks_to_skip.contains_key(s))
            .collect()
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

    // TODO: rename to one of: lift_instructions, lift_range, lift_instruction_range, lift_block?
    fn lift_instruction(&mut self, start: usize, end: usize, statements: &mut Vec<Statement>) {
        let mut top: Option<(ast::RValue, u8)> = None;
        let mut iter = self.bytecode.code[start..=end].iter();
        while let Some(instruction) = iter.next() {
            match instruction {
                Instruction::Move {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![self.locals[source].clone().into()],
                        )
                        .into(),
                    );
                }
                &Instruction::LoadBoolean {
                    destination, value, ..
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Literal::Boolean(value).into()],
                        )
                        .into(),
                    );
                }
                &Instruction::LoadConstant {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![self.constant(source).into()],
                        )
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
                Instruction::Length {
                    destination,
                    operand,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Unary::new(
                                self.locals[operand].clone().into(),
                                ast::UnaryOperation::Length,
                            )
                            .into()],
                        )
                        .into(),
                    );
                }
                Instruction::Minus {
                    destination,
                    operand,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Unary::new(
                                self.locals[operand].clone().into(),
                                ast::UnaryOperation::Negate,
                            )
                            .into()],
                        )
                        .into(),
                    );
                }
                &Instruction::Return(values, b) => {
                    let values = if b != 0 {
                        (values.0..values.0 + (b - 1) as u8)
                            .map(|r| self.locals[&Register(r)].clone().into())
                            .collect()
                    } else {
                        let (tail, end) = top.take().unwrap();
                        (values.0..end)
                            .map(|r| self.locals[&Register(r)].clone().into())
                            .chain(std::iter::once(tail))
                            .collect()
                    };
                    statements.push(ast::Return::new(values).into());
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
                &Instruction::LessThan {
                    lhs,
                    rhs,
                    comparison_value,
                } => {
                    let lhs = self.register_or_constant(lhs);
                    let rhs = self.register_or_constant(rhs);
                    let value = ast::Binary::new(lhs, rhs, ast::BinaryOperation::LessThan).into();
                    let condition = if comparison_value {
                        value
                    } else {
                        ast::Unary::new(value, ast::UnaryOperation::Not).into()
                    };
                    statements.push(ast::If::new(condition, None, None).into())
                }
                &Instruction::Equal {
                    lhs,
                    rhs,
                    comparison_value,
                } => {
                    let lhs = self.register_or_constant(lhs);
                    let rhs = self.register_or_constant(rhs);
                    let value = ast::Binary::new(lhs, rhs, ast::BinaryOperation::Equal).into();
                    let condition = if comparison_value {
                        value
                    } else {
                        ast::Unary::new(value, ast::UnaryOperation::Not).into()
                    };
                    statements.push(ast::If::new(condition, None, None).into())
                }
                Instruction::TestSet {
                    destination,
                    value,
                    comparison_value,
                } => {
                    let value: ast::RValue = self.locals[value].clone().into();
                    let assign = ast::Assign::new(
                        vec![self.locals[destination].clone().into()],
                        vec![value.clone()],
                    );
                    let new_block = self.function.new_block();

                    statements.push(
                        ast::If {
                            condition: if *comparison_value {
                                ast::Unary {
                                    value: Box::new(value.clone()),
                                    operation: ast::UnaryOperation::Not,
                                }
                                .into()
                            } else {
                                value.clone()
                            },
                            then_block: None,
                            else_block: None,
                        }
                        .into(),
                    );

                    let condition_block = self.nodes[&start];
                    let next_block = self.nodes[&(end + 1)];
                    let skip = match &self.bytecode.code[end] {
                        Instruction::Jump(skip) => *skip as usize,
                        _ => unreachable!(),
                    };

                    self.function
                        .block_mut(new_block)
                        .unwrap()
                        .ast
                        .push(assign.into());

                    self.function.set_block_terminator(
                        condition_block,
                        Some(Terminator::conditional(next_block, new_block)),
                    );

                    self.function.set_block_terminator(
                        new_block,
                        Some(Terminator::jump(
                            self.get_node(&(end + skip as usize - 131070)),
                        )),
                    );
                }
                &Instruction::TailCall {
                    function,
                    arguments,
                } => {
                    top = Some((
                        ast::RValue::Variadic(
                            ast::Call::new(
                                self.locals[&function].clone().into(),
                                (1..arguments)
                                    .map(|argument| {
                                        self.locals[&Register(function.0 + argument)].clone().into()
                                    })
                                    .collect(),
                            )
                            .into(),
                        ),
                        function.0,
                    ));
                }
                &Instruction::Call {
                    function,
                    arguments,
                    return_values,
                } => {
                    /*let call = ast::Call {
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
                        ast::Assign::new(
                            (0..return_values - 1)
                                .map(|return_value| {
                                    self.locals[&Register(function.0 + return_value)]
                                        .clone()
                                        .into()
                                })
                                .collect_vec(),
                            vec![call.into()],
                        )
                        .into()
                    } else {
                        call.into()
                    })*/

                    let arguments = if arguments != 0 {
                        (function.0 + 1..function.0 + arguments)
                            .map(|r| self.locals[&Register(r)].clone().into())
                            .collect()
                    } else {
                        let top = top.take().unwrap();
                        (function.0 + 1..top.1)
                            .map(|r| self.locals[&Register(r)].clone().into())
                            .chain(std::iter::once(top.0))
                            .collect()
                    };

                    let call = ast::Call::new(self.locals[&function].clone().into(), arguments);

                    if return_values == 0 {
                        top = Some((ast::RValue::Variadic(call.into()), function.0));
                    } else if return_values == 1 {
                        statements.push(call.into());
                    } else {
                        statements.push(
                            ast::Assign::new(
                                (function.0..function.0 + return_values - 1)
                                    .map(|r| self.locals[&Register(r)].clone().into())
                                    .collect_vec(),
                                vec![call.into()],
                            )
                            .into(),
                        );
                    }
                }
                Instruction::GetUpvalue {
                    destination,
                    upvalue,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![RcLocal::new(Rc::new(ast::Local(Some(
                                self.bytecode.upvalues[upvalue.0 as usize].to_string(),
                            ))))
                            .into()],
                        )
                        .into(),
                    );
                }
                &Instruction::VarArg(destination, b) => {
                    let vararg = ast::RValue::Variadic(ast::VarArg {}.into());
                    if b != 0 {
                        statements.push(
                            ast::Assign::new(
                                (destination.0..destination.0 + b - 1)
                                    .map(|r| self.locals[&Register(r)].clone().into())
                                    .collect(),
                                vec![vararg],
                            )
                            .into(),
                        );
                    } else {
                        top = Some((vararg, destination.0));
                    }
                }
                Instruction::Closure {
                    destination,
                    function,
                } => {
                    /*let closure = Self::lift(&self.bytecode.closures[function.0 as usize]);
                    let parameters = closure.parameters.clone();
                    let body = restructure::lift(closure);

                    statements.push(
                        ast::Assign {
                            left: vec![(self.locals[destination].clone().into(), None)],
                            right: vec![ast::Closure {
                                parameters,
                                body,
                                upvalues: Vec::new(),
                            }
                            .into()],
                        }
                        .into(),
                    );*/

                    let mut upvalues = Vec::new();
                    for _ in 0..self.bytecode.closures[function.0 as usize].number_of_upvalues {
                        let local = match iter.next().as_ref().unwrap() {
                            Instruction::Move {
                                destination: _,
                                source,
                            } => self.locals[source].clone(),
                            Instruction::GetUpvalue {
                                destination: _,
                                upvalue,
                            } => self.function.upvalues_captured[upvalue.0 as usize].clone(),
                            _ => panic!(),
                        };
                        upvalues.push(local);
                    }

                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Closure {
                                parameters: Vec::new(),
                                body: Default::default(),
                                upvalues,
                            }
                            .into()],
                        )
                        .into(),
                    );
                }
                Instruction::NewTable { destination, .. } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Table::default().into()],
                        )
                        .into(),
                    );
                }
                &Instruction::SetList {
                    table,
                    number_of_elements,
                    block_number,
                } => {
                    const FIELDS_PER_FLUSH: usize = 50;

                    let setlist = if number_of_elements != 0 {
                        ast::SetList::new(
                            self.locals[&table].clone(),
                            (block_number - 1) as usize * FIELDS_PER_FLUSH + 1,
                            (table.0 + 1..table.0 + 1 + number_of_elements)
                                .map(|r| self.locals[&Register(r)].clone().into())
                                .collect(),
                            None,
                        )
                    } else {
                        let top = top.take().unwrap();
                        ast::SetList::new(
                            self.locals[&table].clone(),
                            (block_number - 1) as usize * FIELDS_PER_FLUSH + 1,
                            (table.0 + 1..top.1)
                                .map(|r| self.locals[&Register(r)].clone().into())
                                .collect(),
                            Some(top.0),
                        )
                    };
                    statements.push(setlist.into());
                }
                Instruction::Close(start) => {
                    let locals = (start.0..self.bytecode.maximum_stack_size)
                        .map(|i| self.locals[&Register(i)].clone())
                        .collect();
                    statements.push(ast::Close { locals }.into());
                }
                &Instruction::SetTable { table, key, value } => {
                    let key = self.register_or_constant(key);
                    let value = self.register_or_constant(value);

                    statements.push(
                        ast::Assign::new(
                            vec![ast::Index {
                                left: Box::new(self.locals[&table].clone().into()),
                                right: Box::new(key),
                            }
                            .into()],
                            vec![value],
                        )
                        .into(),
                    );
                }
                Instruction::PrepareNumericForLoop { .. } => {}
                Instruction::IterateNumericForLoop { control, skip } => {
                    let (internal_counter, limit, step, external_counter) = (
                        self.locals[&control[0]].clone(),
                        self.locals[&control[1]].clone(),
                        self.locals[&control[2]].clone(),
                        self.locals[&control[3]].clone(),
                    );
                    statements.push(
                        ast::NumForNext::new(internal_counter.clone(), limit.into(), step.into())
                            .into(),
                    );
                    self.function
                        .block_mut(self.get_node(&(end + *skip as usize - 131070)))
                        .unwrap()
                        .ast
                        .insert(
                            0,
                            ast::Assign::new(
                                vec![external_counter.into()],
                                vec![internal_counter.into()],
                            )
                            .into(),
                        );
                }
                _ => statements.push(ast::Comment::new(format!("{:?}", instruction)).into()),
            }

            if matches!(instruction, Instruction::Return { .. }) {
                break;
            }
        }
    }

    fn get_node(&'a self, mut index: &'a usize) -> NodeIndex {
        while let Some(index_to) = self.blocks_to_skip.get(index) {
            index = index_to;
        }
        self.nodes[index]
    }

    fn lift_blocks(&mut self) {
        let ranges = self.code_ranges();
        for (start, end) in ranges {
            // TODO: gotta be a better way
            // we need to do this in case that the body of a for loop is after the for loop instruction
            // see: IterateNumericForLoop
            let mut statements =
                std::mem::take(&mut self.function.block_mut(self.nodes[&start]).unwrap().ast);
            self.lift_instruction(start, end, &mut statements);
            self.function.block_mut(self.nodes[&start]).unwrap().ast = statements;

            match self.bytecode.code[end] {
                Instruction::Equal { .. }
                | Instruction::LessThan { .. }
                | Instruction::LessThanOrEqual { .. }
                | Instruction::Test { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::conditional(
                            self.get_node(&(end + 1)),
                            self.get_node(&(end + 2)),
                        )),
                    );
                }
                Instruction::IterateNumericForLoop { skip, .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::conditional(
                            self.get_node(&(end + skip as usize - 131070)),
                            self.get_node(&(end + 1)),
                        )),
                    );
                }
                Instruction::Jump(skip) | Instruction::PrepareNumericForLoop { skip, .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::jump(
                            self.get_node(&(end + skip as usize - 131070)),
                        )),
                    );
                }
                Instruction::Return { .. } => {}
                Instruction::LoadBoolean { skip_next, .. } => {
                    let successor = self.get_node(&(end + 1 + skip_next as usize));

                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::jump(successor)),
                    );
                }
                _ => {
                    if end + 1 != self.bytecode.code.len() {
                        self.function.set_block_terminator(
                            self.nodes[&start],
                            Some(Terminator::jump(self.get_node(&(end + 1)))),
                        );
                    }
                }
            }
        }
    }

    pub fn lift(bytecode: &'a BytecodeFunction) -> Function {
        let mut context = Self {
            bytecode,
            nodes: FxHashMap::default(),
            locals: FxHashMap::default(),
            constants: FxHashMap::default(),
            function: Function::default(),
            blocks_to_skip: FxHashMap::default(),
        };

        context.create_block_map();
        context.allocate_locals();
        context.lift_blocks();
        for node in context
            .function
            .graph()
            .node_indices()
            .filter(|&i| i != context.nodes[&0])
            .collect::<Vec<_>>()
        {
            if context.function.predecessor_blocks(node).next().is_none() {
                context.function.remove_block(node);
            }
        }
        context.function.set_entry(context.nodes[&0]);

        let dominators = simple_fast(context.function.graph(), context.function.entry().unwrap());
        for node in context.function.graph().node_indices().collect_vec() {
            let mut successors = context.function.successor_blocks(node);
            if let Some(target) = successors.next() && successors.next().is_none() && context.function.predecessor_blocks(target).count() == 1
            && dominators.dominators(target).unwrap().contains(&node) && target != node {
                let block = context.function.remove_block(target).unwrap();
                let terminator = block.terminator;
                context.function
                    .block_mut(node)
                    .unwrap()
                    .ast
                    .extend(block.ast.0);
                context.function.set_block_terminator(node, terminator);
            }
        }

        context.function
    }
}
