use std::{cell::RefCell, rc::Rc};

use either::Either;
use fxhash::FxHashMap;
use indexmap::IndexMap;
use itertools::Itertools;

use ast::{local_allocator::LocalAllocator, replace_locals::replace_locals, RcLocal, Statement};
use cfg::{
    block::Terminator,
    function::Function,
    inline::inline,
    ssa::structuring::{
        structure_compound_conditionals, structure_for_loops, structure_jumps,
        structure_method_calls,
    },
};

use lua51_deserializer::{
    argument::{Constant, Register, RegisterOrConstant},
    Function as BytecodeFunction, Instruction, Value,
};

use petgraph::{algo::dominators::simple_fast, stable_graph::NodeIndex};
use restructure::post_dominators;

pub struct LifterContext<'a> {
    bytecode: &'a BytecodeFunction<'a>,
    nodes: FxHashMap<usize, NodeIndex>,
    blocks_to_skip: FxHashMap<usize, usize>,
    locals: FxHashMap<Register, RcLocal>,
    constants: FxHashMap<usize, ast::Literal>,
    function: Function,
    upvalues: Vec<RcLocal>,
}

impl<'a> LifterContext<'a> {
    fn allocate_locals(&mut self) {
        for _ in 0..self.bytecode.number_of_upvalues {
            self.upvalues
                .push(self.function.local_allocator.borrow_mut().allocate());
        }

        for i in 0..self.bytecode.maximum_stack_size {
            let local = self.function.local_allocator.borrow_mut().allocate();
            if i < self.bytecode.number_of_parameters {
                self.function.parameters.push(local.clone());
            }
            self.locals.insert(Register(i), local);
        }
    }

    // TODO: support jumps to invalid destinations
    // including cases where there is usize::MAX instructions and the last instruction
    // skips forward, overflowing
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
                | Instruction::TestSet { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                    self.nodes
                        .entry(insn_index + 2)
                        .or_insert_with(|| self.function.new_block());
                }
                Instruction::Jump(skip) => {
                    let dest_index = (insn_index + 1).checked_add_signed(skip.try_into().unwrap()).unwrap();
                    /* let dest_block = * */self
                        .nodes
                        .entry(dest_index)
                        .or_insert_with(|| self.function.new_block());
                    self.nodes
                        .entry(insn_index + 1)
                        .or_insert_with(|| self.function.new_block());
                    // UNCOMMENTING THIS BREAKS
                    // for i in t do end
                    // BETTER TO JUST LEAVE IT COMMENTED
                    // if insn_index != dest_index {
                    //     if let Some(jmp_block) = self.nodes.remove(&insn_index) {
                    //         self.function.remove_block(jmp_block);
                    //         self.nodes.insert(insn_index, dest_block);
                    //         self.blocks_to_skip.insert(insn_index, dest_index);
                    //     }
                    // }
                }
                Instruction::IterateNumericForLoop { skip, .. }
                | Instruction::InitNumericForLoop { skip, .. } => {
                    self.nodes
                        .entry((insn_index + 1).checked_add_signed(skip.try_into().unwrap()).unwrap())
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
            Value::String(v) => ast::Literal::String(v.to_vec()),
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
        if end > start {
            statements.reserve(end - start + 1);
        }
        let mut top: Option<(ast::RValue, u8)> = None;
        // TODO: we should consume the instructions, reducing clones
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
                &Instruction::GetIndex {
                    destination,
                    object,
                    key,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Index::new(
                                self.locals[&object].clone().into(),
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
                Instruction::Concatenate {
                    destination,
                    operands,
                } => {
                    assert!(operands.len() >= 2);
                    let mut concat = ast::Binary::new(
                        self.locals[&operands[0]].clone().into(),
                        self.locals[&operands[1]].clone().into(),
                        ast::BinaryOperation::Concat,
                    );
                    for r in operands.iter().skip(2) {
                        concat = ast::Binary::new(
                            concat.into(),
                            self.locals[r].clone().into(),
                            ast::BinaryOperation::Concat,
                        );
                    }
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![concat.into()],
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
                &Instruction::LessThanOrEqual {
                    lhs,
                    rhs,
                    comparison_value,
                } => {
                    let lhs = self.register_or_constant(lhs);
                    let rhs = self.register_or_constant(rhs);
                    let value =
                        ast::Binary::new(lhs, rhs, ast::BinaryOperation::LessThanOrEqual).into();
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

                    let assign = ast::Assign::new(
                        vec![self.locals[destination].clone().into()],
                        vec![value.clone()],
                    );

                    self.function
                        .block_mut(self.nodes[&(end + 1)])
                        .unwrap()
                        .ast
                        .push(assign.into());
                }
                &Instruction::PrepMethodCall {
                    destination,
                    self_arg,
                    object,
                    method,
                } => {
                    let destination = self.locals[&destination].clone();
                    let self_arg = self.locals[&self_arg].clone();
                    let object = self.locals[&object].clone();
                    statements.push(
                        ast::Assign::new(vec![self_arg.into()], vec![object.clone().into()]).into(),
                    );
                    statements.push(
                        ast::Assign::new(
                            vec![destination.into()],
                            vec![
                                ast::Index::new(object.into(), self.register_or_constant(method))
                                    .into(),
                            ],
                        )
                        .into(),
                    );
                }
                &Instruction::TailCall {
                    function,
                    arguments,
                }
                | &Instruction::Call {
                    function,
                    arguments,
                    ..
                } => {
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

                    if let &Instruction::Call { return_values, .. } = instruction
                        && return_values != 0
                    {
                        if return_values == 1 {
                            statements.push(call.into());
                        } else {
                            statements.push(
                                ast::Assign::new(
                                    (function.0..function.0 + return_values - 1)
                                        .map(|r| self.locals[&Register(r)].clone().into())
                                        .collect_vec(),
                                    vec![ast::RValue::Select(call.into())],
                                )
                                .into(),
                            );
                        }
                    } else {
                        top = Some((call.into(), function.0));
                    }
                }
                Instruction::GetUpvalue {
                    destination,
                    upvalue,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![self.upvalues[upvalue.0 as usize].clone().into()],
                        )
                        .into(),
                    );
                }
                Instruction::SetUpvalue {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign::new(
                            vec![self.upvalues[destination.0 as usize].clone().into()],
                            vec![self.locals[source].clone().into()],
                        )
                        .into(),
                    );
                }
                &Instruction::VarArg(destination, b) => {
                    let vararg = ast::VarArg {};
                    if b != 0 {
                        statements.push(
                            ast::Assign::new(
                                (destination.0..destination.0 + b - 1)
                                    .map(|r| self.locals[&Register(r)].clone().into())
                                    .collect(),
                                vec![ast::RValue::Select(vararg.into())],
                            )
                            .into(),
                        );
                    } else {
                        top = Some((vararg.into(), destination.0));
                    }
                }
                Instruction::Closure {
                    destination,
                    function,
                } => {
                    let closure = &self.bytecode.closures[function.0 as usize];

                    let mut upvalues_passed = Vec::new();
                    for _ in 0..closure.number_of_upvalues {
                        let local = match iter.next().as_ref().unwrap() {
                            Instruction::Move {
                                destination: _,
                                source,
                            } => self.locals[source].clone(),
                            Instruction::GetUpvalue {
                                destination: _,
                                upvalue,
                            } => self.upvalues[upvalue.0 as usize].clone(),
                            _ => panic!(),
                        };
                        upvalues_passed.push(local);
                    }

                    let (mut body, parameters, upvalues_in) =
                        Self::lift(closure, self.function.local_allocator.clone());
                    let mut local_map =
                        FxHashMap::with_capacity_and_hasher(upvalues_in.len(), Default::default());
                    for (old, new) in upvalues_in.into_iter().zip(&upvalues_passed) {
                        //println!("{} -> {}", old, new);
                        local_map.insert(old, new.clone());
                    }
                    replace_locals(&mut body, &local_map);

                    statements.push(
                        ast::Assign::new(
                            vec![self.locals[destination].clone().into()],
                            vec![ast::Closure {
                                parameters,
                                body,
                                upvalues: upvalues_passed,
                                is_variadic: closure.vararg_flag != 0,
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
                &Instruction::SetIndex { object, key, value } => {
                    let key = self.register_or_constant(key);
                    let value = self.register_or_constant(value);

                    statements.push(
                        ast::Assign::new(
                            vec![ast::Index {
                                left: Box::new(self.locals[&object].clone().into()),
                                right: Box::new(key),
                            }
                            .into()],
                            vec![value],
                        )
                        .into(),
                    );
                }
                Instruction::InitNumericForLoop { control, .. } => {
                    let (internal_counter, limit, step) = (
                        self.locals[&control[0]].clone(),
                        self.locals[&control[1]].clone(),
                        self.locals[&control[2]].clone(),
                    );
                    statements.push(ast::NumForInit::new(internal_counter, limit, step).into());
                }
                &Instruction::IterateNumericForLoop { ref control, skip } => {
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
                        .block_mut(self.get_node(&((end + 1).checked_add_signed(skip.try_into().unwrap()).unwrap())))
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
                Instruction::IterateGenericForLoop {
                    generator,
                    state,
                    internal_control,
                    vars,
                } => {
                    let generator = self.locals[generator].clone();
                    let state = self.locals[state].clone();
                    let internal_control = self.locals[internal_control].clone();
                    let vars = vars
                        .iter()
                        .map(|x| self.locals[x].clone())
                        .collect::<Vec<_>>();
                    let control = vars[0].clone();
                    statements.push(
                        ast::Assign::new(
                            vars.into_iter().map(|l| l.into()).collect(),
                            vec![ast::Call::new(
                                generator.clone().into(),
                                vec![state.clone().into(), internal_control.clone().into()],
                            )
                            .into()],
                        )
                        .into(),
                    );
                    statements.push(ast::If::new(control.clone().into(), None, None).into());

                    let body_node = self.get_node(&(end + 1));
                    self.function.block_mut(body_node).unwrap().ast.insert(
                        0,
                        ast::Assign::new(
                            vec![internal_control.clone().into()],
                            vec![control.clone().into()],
                        )
                        .into(),
                    );
                }
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
                | Instruction::TestSet { .. }
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
                            self.get_node(&((end + 1).checked_add_signed(skip.try_into().unwrap()).unwrap())),
                            self.get_node(&(end + 1)),
                        )),
                    );
                }
                Instruction::Jump(skip) | Instruction::InitNumericForLoop { skip, .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::jump(
                            self.get_node(&((end + 1).checked_add_signed(skip.try_into().unwrap()).unwrap())),
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

    pub fn lift(
        bytecode: &'a BytecodeFunction,
        local_allocator: Rc<RefCell<LocalAllocator>>,
    ) -> (ast::Block, Vec<RcLocal>, Vec<RcLocal>) {
        let mut context = Self {
            bytecode,
            nodes: FxHashMap::default(),
            blocks_to_skip: FxHashMap::default(),
            locals: FxHashMap::default(),
            constants: FxHashMap::default(),
            function: Function::with_allocator(local_allocator),
            upvalues: Vec::new(),
        };

        context.create_block_map();
        context.allocate_locals();
        context.lift_blocks();

        context.function.set_entry(context.nodes[&0]);

        // merge blocks where possible
        // let dominators = simple_fast(context.function.graph(), context.function.entry().unwrap());
        // for node in context.function.graph().node_indices().collect_vec() {
        //     let mut successors = context.function.successor_blocks(node);
        //     if let Some(target) = successors.next() && successors.next().is_none() && context.function.predecessor_blocks(target).count() == 1
        //     && dominators.dominators(target).unwrap().contains(&node) && target != node {
        //         let block = context.function.remove_block(target).unwrap();
        //         let terminator = block.terminator;
        //         context.function
        //             .block_mut(node)
        //             .unwrap()
        //             .ast
        //             .extend(block.ast.0);
        //         context.function.set_block_terminator(node, terminator);
        //     }
        // }

        let func = |mut function, upvalues_in| {
            let (local_count, local_groups, upvalue_groups) =
                cfg::ssa::construct(&mut function, &upvalues_in);

            //cfg::dot::render_to(&function, &mut std::io::stdout())?;

            let upvalue_to_group = upvalue_groups
                .iter()
                .cloned()
                .enumerate()
                .flat_map(|(i, g)| g.into_iter().map(move |u| (u, i)))
                .collect::<IndexMap<_, _>>();

            loop {
                let dominators = simple_fast(function.graph(), function.entry().unwrap());
                structure_jumps(&mut function, &dominators);

                // TODO: remove unused variables without side effects
                inline(&mut function, &upvalue_to_group);

                if !structure_compound_conditionals(&mut function)
                    && !{
                        let post_dominators = post_dominators(function.graph().clone());
                        structure_for_loops(&mut function, &dominators, &post_dominators)
                    }
                    && !structure_method_calls(&mut function)
                {
                    break;
                }

                let mut local_map = FxHashMap::default();
                cfg::ssa::construct::remove_unnecessary_params(&mut function, &mut local_map);
                cfg::ssa::construct::apply_local_map(&mut function, local_map);
            }
            //cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();

            //let dataflow = cfg::ssa::dataflow::DataFlow::new(&function);
            //println!("dataflow: {:#?}", dataflow);

            //cfg::dot::render_to(&function, &mut std::io::stdout()).unwrap();

            let upvalues_in = cfg::ssa::destruct(
                &mut function,
                local_count,
                &local_groups,
                &upvalue_to_group,
                upvalues_in.len(),
            )
            .into_iter()
            .collect::<Vec<_>>();

            // cfg::dot::render_to(&function, &mut std::io::stdout())?;
            let params = function.parameters.clone();
            (restructure::lift(function.clone()), params, upvalues_in)
        };

        match () {
            #[cfg(feature = "panic_handled")]
            () => {
                let mut function = std::panic::AssertUnwindSafe(Some(context.function));
                let mut upvalues_in = std::panic::AssertUnwindSafe(Some(context.upvalues));
                let result = std::panic::catch_unwind(move || {
                    func(function.take().unwrap(), upvalues_in.take().unwrap())
                });
                match result {
                    Ok(r) => r,
                    Err(e) => {
                        let panic_information = match e.downcast::<String>() {
                            Ok(v) => *v,
                            Err(e) => match e.downcast::<&str>() {
                                Ok(v) => v.to_string(),
                                _ => "Unknown Source of Error".to_owned(),
                            },
                        };

                        let mut block = ast::Block::from_vec(vec![ast::Comment::new(
                            "the decompiler panicked while decompiling this function".to_string(),
                        )
                        .into()]);

                        for line in panic_information.split('\n') {
                            block.push(ast::Comment::new(line.to_string()).into());
                        }

                        (block, Vec::new(), Vec::new())
                    }
                }
            }
            #[cfg(not(feature = "panic_handled"))]
            () => func(context.function, context.upvalues),
        }
    }
}
