use std::{borrow::Cow, rc::Rc};

use either::Either;
use fxhash::FxHashMap;

use ast::RcLocal;
use cfg::{block::Terminator, function::Function};
use graph::NodeId;
use lua51_deserializer::argument::{Constant, Register, RegisterOrConstant};
use lua51_deserializer::{Function as BytecodeFunction, Instruction, Value};

struct LifterContext<'a> {
    bytecode: &'a BytecodeFunction<'a>,
    nodes: FxHashMap<usize, NodeId>,
    locals: FxHashMap<Register, RcLocal<'a>>,
    constants: FxHashMap<usize, ast::Literal<'a>>,
    function: Function<'a>,
}

impl<'a> LifterContext<'a> {
    fn allocate_locals(&mut self) {
        for i in 0..self.bytecode.maximum_stack_size {
            self.locals.insert(
                Register(i),
                RcLocal::new(Rc::new(ast::Local::new(format!("l_{}", i).into()))),
            );
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

    fn constant(&mut self, constant: Constant) -> ast::Literal<'a> {
        let converted_constant = match self.bytecode.constants.get(constant.0 as usize).unwrap() {
            Value::Nil => ast::Literal::Nil,
            Value::Boolean(v) => ast::Literal::Boolean(*v),
            Value::Number(v) => ast::Literal::Number(*v),
            Value::String(v) => ast::Literal::String(Cow::Borrowed(v)),
        };
        self.constants
            .entry(constant.0 as usize)
            .or_insert(converted_constant)
            .clone()
    }

    fn register_or_constant(&mut self, value: RegisterOrConstant) -> ast::RValue<'a> {
        match value.0 {
            Either::Left(register) => self.locals[&register].clone().into(),
            Either::Right(constant) => self.constant(constant).into(),
        }
    }

    fn lift_instruction(
        &mut self,
        start: usize,
        end: usize,
        statements: &mut Vec<ast::Statement<'a>>,
    ) {
        let mut iterator = start..=end;

        while let Some(index) = iterator.next() {
            let instruction = self.bytecode.code[index].clone();
            match instruction {
                Instruction::Move {
                    destination,
                    source,
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[&destination].clone().into()],
                            right: vec![self.locals[&source].clone().into()],
                        }
                        .into(),
                    );
                }
                Instruction::LoadBoolean {
                    destination,
                    value,
                    skip_next,
                } => {
                    statements.push(
                        ast::Assign {
                            left: vec![self.locals[&destination].clone().into()],
                            right: vec![ast::Literal::Boolean(value).into()],
                        }
                        .into(),
                    );

                    if skip_next {
                        iterator.next();
                    }
                }
                Instruction::LoadConstant {
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
                                vec![self.locals[&register].clone().into()],
                                vec![ast::Literal::Nil.into()],
                            )
                            .into(),
                        );
                    }
                }
                Instruction::GetGlobal {
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
                Instruction::SetGlobal { destination, value } => {
                    let global_str = self.constant(destination).as_string().unwrap().clone();
                    statements.push(
                        ast::Assign::new(
                            vec![ast::Global::new(global_str).into()],
                            vec![self.locals[&value].clone().into()],
                        )
                        .into(),
                    );
                }
                Instruction::Test {
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
                            vec![self.locals[&destination].clone().into()],
                            vec![ast::Unary::new(
                                self.locals[&operand].clone().into(),
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
                                .into_iter()
                                .map(|v| self.locals[&v].clone().into())
                                .collect(),
                        )
                        .into(),
                    );
                }
                Instruction::Jump(..) => {}
                Instruction::Add {
                    destination,
                    lhs,
                    rhs,
                }
                | Instruction::Sub {
                    destination,
                    lhs,
                    rhs,
                }
                | Instruction::Mul {
                    destination,
                    lhs,
                    rhs,
                }
                | Instruction::Div {
                    destination,
                    lhs,
                    rhs,
                }
                | Instruction::Mod {
                    destination,
                    lhs,
                    rhs,
                }
                | Instruction::Pow {
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
                _ => statements.push(ast::Comment::new(format!("{:?}", instruction)).into()),
            }

            if matches!(self.bytecode.code[index], Instruction::Return { .. }) {
                break;
            }
        }
    }

    fn lift_blocks(&mut self) {
        let ranges = self.code_ranges();
        for (start, end) in ranges {
            let mut block = ast::Block::new();

            self.lift_instruction(start, end, &mut block);

            match self.bytecode.code[end] {
                Instruction::Equal { .. }
                | Instruction::LessThan { .. }
                | Instruction::LessThanOrEqual { .. }
                | Instruction::Test { .. }
                | Instruction::IterateGenericForLoop { .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::conditional(
                            self.nodes[&(end + 1)],
                            self.nodes[&(end + 2)],
                        )),
                    );
                }
                Instruction::Jump(step)
                | Instruction::IterateNumericForLoop { step, .. }
                | Instruction::PrepareNumericForLoop { step, .. } => {
                    self.function.set_block_terminator(
                        self.nodes[&start],
                        Some(Terminator::jump(
                            self.nodes[&(end + step as usize - 131070)],
                        )),
                    );
                }
                Instruction::Return { .. } => {}
                _ => {
                    if end + 1 != self.bytecode.code.len() {
                        self.function.set_block_terminator(
                            self.nodes[&start],
                            Some(Terminator::jump(self.nodes[&(end + 1)])),
                        );
                    }
                }
            }
            self.function
                .block_mut(self.nodes[&start])
                .unwrap()
                .extend(block.0);
        }
    }

    fn lift(mut self) -> Function<'a> {
        self.create_block_map();
        self.allocate_locals();
        self.lift_blocks();
        self.function.set_entry(self.nodes[&0]);
        self.function
    }
}

pub fn lift<'a>(bytecode: &'a BytecodeFunction<'a>) -> Function<'a> {
    let context = LifterContext {
        bytecode,
        nodes: FxHashMap::default(),
        locals: FxHashMap::default(),
        constants: FxHashMap::default(),
        function: Function::default(),
    };
    context.lift()
}

/*pub struct Lifter<'a> {
    bytecode_function: &'a BytecodeFunction<'a>,
    blocks: FxHashMap<usize, NodeId>,
    lifted_function: Function<'a>,
    // TODO: make this a ref
    lifted_descendants: Vec<Rc<RefCell<Function<'a>>>>,
    closures: Vec<Option<Rc<RefCell<Function<'a>>>>>,
    register_map: FxHashMap<Register, RcLocal<'a>>,
    constant_map: FxHashMap<usize, ast::Literal<'a>>,
    current_node: Option<NodeId>,
}

impl<'a> Lifter<'a> {
    pub fn new(function: &'a BytecodeFunction<'a>) -> Self {
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

    pub fn lift_blocka(&'a mut self, start_pc: usize, end_pc: usize) {
        let (statements, terminator) = self.lift_block(start_pc, end_pc);
        let current_block = self.block_to_node(start_pc);
        let lifted_function = &mut self.lifted_function;
        let block = lifted_function
            .block_mut(current_block)
            .unwrap();
        block.extend(statements);
        lifted_function.set_block_terminator(current_block, Some(terminator));
    }

    pub fn lift_function(&'a mut self) -> Result<Function<'a>> {
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
            self.lift_blocka(start_pc, end_pc);
        }

        /*for (start_pc, end_pc) in block_ranges {
            self.current_node = Some(self.block_to_node(start_pc));
            let (statements, terminator) = self.lift_block(start_pc, end_pc);
            let current_node = self.current_node.unwrap();
            let block = &'a mut self
                .lifted_function
                .block_mut(current_node)
                .unwrap()
                .block;
            block.extend(statements);
            self.lifted_function
                .set_block_terminator(self.current_node.unwrap(), Some(terminator))
                .unwrap();
        }*/

        self.lifted_function.set_entry(self.block_to_node(0))?;

        Ok(self.lifted_function)
    }

    fn discover_blocks(&'a mut self) -> Result<()> {
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
        &'a mut self,
        block_start: usize,
        block_end: usize,
    ) -> (Vec<ast::Statement<'a>>, Terminator<'a>) {
        let mut statements = Vec::new();
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
                    statements.push(
                        ast::Assign {
                            left: vec![self.register(destination).into()],
                            right: vec![self.register(source).into()],
                        }
                        .into(),
                    );
                }
                other => unimplemented!("{:?}", other),
            }
        }
        (
            statements,
            terminator.unwrap_or_else(|| Terminator::jump(self.block_to_node(block_end + 1))),
        )
    }

    fn register_or_constant(
        &'a mut self,
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

    fn register(&'a mut self, register: Register) -> RcLocal<'a> {
        self.register_map
            .entry(register)
            .or_insert_with(|| self.lifted_function.local_allocator.allocate())
            .clone()
    }

    fn global(&'a mut self, constant: Constant) -> ast::Global<'a> {
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

    fn constant(&'a mut self, constant: Constant) -> ast::Literal<'a> {
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
}*/
