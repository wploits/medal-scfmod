use std::{cell::RefCell, rc::Rc};

use anyhow::Result;

use petgraph::stable_graph::NodeIndex;
use rustc_hash::FxHashMap;

use super::{
    deserializer::{
        constant::Constant as BytecodeConstant, function::Function as BytecodeFunction,
    },
    instruction::Instruction,
    op_code::OpCode,
};
use ast;
use cfg::{
    block::{BlockEdge, BranchType},
    function::Function,
};

pub struct Lifter<'a> {
    function: usize,
    function_list: &'a Vec<BytecodeFunction>,
    string_table: &'a Vec<String>,
    blocks: FxHashMap<usize, NodeIndex>,
    lifted_function: Function,
    // TODO: make this a ref
    lifted_descendants: Vec<Rc<RefCell<Function>>>,
    closures: FxHashMap<usize, Rc<RefCell<Function>>>,
    register_map: FxHashMap<usize, ast::RcLocal>,
    constant_map: FxHashMap<usize, ast::Literal>,
    current_node: Option<NodeIndex>,
}

impl<'a> Lifter<'a> {
    pub fn new(
        f_list: &'a Vec<BytecodeFunction>,
        str_list: &'a Vec<String>,
        function_id: usize,
    ) -> Self {
        Self {
            function: function_id,
            function_list: f_list,
            string_table: str_list,
            blocks: FxHashMap::default(),
            lifted_function: Function::default(),
            closures: FxHashMap::default(),
            register_map: FxHashMap::default(),
            constant_map: FxHashMap::default(),
            lifted_descendants: Vec::new(),
            current_node: None,
        }
    }

    pub fn lift_function(mut self) -> Result<Function> {
        self.discover_blocks()?;

        let mut blocks = self.blocks.keys().cloned().collect::<Vec<_>>();

        blocks.sort_unstable();

        let block_ranges = blocks
            .iter()
            .rev()
            .fold(
                (
                    self.function_list[self.function].instructions.len(),
                    Vec::new(),
                ),
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
            let parameter = self.lifted_function.local_allocator.borrow_mut().allocate();
            self.lifted_function.parameters.push(parameter.clone());
            self.register_map.insert(i as usize, parameter);
        }

        for (start_pc, end_pc) in block_ranges {
            self.current_node = Some(self.block_to_node(start_pc));
            let (statements, edges) = self.lift_block(start_pc, end_pc);
            let block = self
                .lifted_function
                .block_mut(self.current_node.unwrap())
                .unwrap();
            block.0.extend(statements);
            self.lifted_function
                .set_edges(self.current_node.unwrap(), edges);
        }

        self.lifted_function.set_entry(self.block_to_node(0));

        Ok(self.lifted_function)
    }

    fn discover_blocks(&mut self) -> Result<()> {
        self.blocks.insert(0, self.lifted_function.new_block());
        for (insn_index, insn) in self.function_list[self.function]
            .instructions
            .iter()
            .enumerate()
        {
            match insn {
                Instruction::BC { op_code, c, .. } => match op_code {
                    OpCode::LOP_LOADB if *c != 0 => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*c as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    _ => {}
                },

                Instruction::AD {
                    op_code,
                    a: _,
                    d,
                    aux: _,
                } => match op_code {
                    OpCode::LOP_JUMP | OpCode::LOP_JUMPBACK => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    OpCode::LOP_JUMPIF
                    | OpCode::LOP_JUMPIFNOT
                    | OpCode::LOP_JUMPIFEQ
                    | OpCode::LOP_JUMPIFLE
                    | OpCode::LOP_JUMPIFLT
                    | OpCode::LOP_JUMPIFNOTEQ
                    | OpCode::LOP_JUMPIFNOTLE
                    | OpCode::LOP_JUMPIFNOTLT
                    | OpCode::LOP_JUMPIFEQK
                    | OpCode::LOP_JUMPIFNOTEQK => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    OpCode::LOP_FORNPREP
                    | OpCode::LOP_FORGPREP
                    | OpCode::LOP_FORGPREP_NEXT
                    | OpCode::LOP_FORGPREP_INEXT => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    OpCode::LOP_FORNLOOP => {
                        self.blocks
                            .entry(insn_index)
                            .or_insert_with(|| self.lifted_function.new_block());
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    OpCode::LOP_FORGLOOP
                    | OpCode::LOP_FORGLOOP_NEXT
                    | OpCode::LOP_FORGLOOP_INEXT => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block());
                    }
                    _ => {}
                },

                Instruction::E { .. } => {}
            }
        }

        Ok(())
    }

    fn lift_block(
        &mut self,
        block_start: usize,
        block_end: usize,
    ) -> (Vec<ast::Statement>, Vec<(NodeIndex, BlockEdge)>) {
        let mut statements = Vec::new();
        let mut edges = Vec::new();

        let mut top: Option<(ast::RValue, u8)> = None;

        let mut iter = self.function_list[self.function].instructions[block_start..=block_end]
            .iter()
            .enumerate();

        while let Some((index, instruction)) = iter.next() {
            match *instruction {
                Instruction::BC {
                    op_code,
                    a,
                    b,
                    c,
                    aux,
                } => match op_code {
                    OpCode::LOP_PREPVARARGS => {}
                    OpCode::LOP_MOVE => {
                        let a = self.register(a as _);
                        let b = self.register(b as _);
                        statements.push(ast::Assign::new(vec![a.into()], vec![b.into()]).into());
                    }
                    OpCode::LOP_LOADNIL => {
                        let target = self.register(a as _);
                        statements.push(
                            ast::Assign::new(vec![target.into()], vec![ast::Literal::Nil.into()])
                                .into(),
                        )
                    }
                    OpCode::LOP_LOADB => {
                        let target = self.register(a as _);
                        statements.push(
                            ast::Assign::new(
                                vec![target.into()],
                                vec![ast::Literal::Boolean(b != 0).into()],
                            )
                            .into(),
                        )
                    }
                    OpCode::LOP_SETGLOBAL => {
                        let value = self.register(a as _);
                        let global_name = self.constant(aux as _).into_string().unwrap();
                        statements.push(
                            ast::Assign::new(
                                vec![ast::Global::new(global_name).into()],
                                vec![value.into()],
                            )
                            .into(),
                        );
                        iter.next();
                    }
                    OpCode::LOP_GETTABLEKS => {
                        let target = self.register(a as _);
                        let table = self.register(b as _);
                        let key = self.constant(aux as _);
                        statements.push(
                            ast::Assign::new(
                                vec![target.into()],
                                vec![ast::Index::new(table.into(), key.into()).into()],
                            )
                            .into(),
                        );
                        iter.next();
                    }
                    OpCode::LOP_GETTABLEN => {
                        let value = self.register(a as _);
                        let table = self.register(b as _);
                        let key = ast::Literal::Number((c as usize + 1) as f64);
                        statements.push(
                            ast::Assign::new(
                                vec![value.into()],
                                vec![ast::Index::new(table.into(), key.into()).into()],
                            )
                            .into(),
                        );
                    }
                    OpCode::LOP_SETTABLEKS => {
                        let value = self.register(a as _);
                        let table = self.register(b as _);
                        let key = self.constant(aux as _);
                        statements.push(
                            ast::Assign::new(
                                vec![ast::Index::new(table.into(), key.into()).into()],
                                vec![value.into()],
                            )
                            .into(),
                        );
                        iter.next();
                    }
                    OpCode::LOP_SETTABLEN => {
                        let value = self.register(a as _);
                        let table = self.register(b as _);
                        let key = ast::Literal::Number((c as usize + 1) as f64);
                        statements.push(
                            ast::Assign::new(
                                vec![ast::Index::new(table.into(), key.into()).into()],
                                vec![value.into()],
                            )
                            .into(),
                        );
                    }
                    OpCode::LOP_ADD
                    | OpCode::LOP_SUB
                    | OpCode::LOP_MUL
                    | OpCode::LOP_DIV
                    | OpCode::LOP_MOD
                    | OpCode::LOP_POW => {
                        let op = match op_code {
                            OpCode::LOP_ADD => ast::BinaryOperation::Add,
                            OpCode::LOP_SUB => ast::BinaryOperation::Sub,
                            OpCode::LOP_MUL => ast::BinaryOperation::Mul,
                            OpCode::LOP_DIV => ast::BinaryOperation::Div,
                            OpCode::LOP_MOD => ast::BinaryOperation::Mod,
                            OpCode::LOP_POW => ast::BinaryOperation::Pow,
                            _ => unreachable!(),
                        };
                        let target = self.register(a as _);
                        let left = self.register(b as _);
                        let right = self.register(c as _);
                        statements.push(
                            ast::Assign::new(
                                vec![target.into()],
                                vec![ast::Binary::new(left.into(), right.into(), op).into()],
                            )
                            .into(),
                        );
                    }
                    OpCode::LOP_ADDK
                    | OpCode::LOP_SUBK
                    | OpCode::LOP_MULK
                    | OpCode::LOP_DIVK
                    | OpCode::LOP_MODK
                    | OpCode::LOP_POWK => {
                        let op = match op_code {
                            OpCode::LOP_ADDK => ast::BinaryOperation::Add,
                            OpCode::LOP_SUBK => ast::BinaryOperation::Sub,
                            OpCode::LOP_MULK => ast::BinaryOperation::Mul,
                            OpCode::LOP_DIVK => ast::BinaryOperation::Div,
                            OpCode::LOP_MODK => ast::BinaryOperation::Mod,
                            OpCode::LOP_POWK => ast::BinaryOperation::Pow,
                            _ => unreachable!(),
                        };
                        let target = self.register(a as _);
                        let left = self.register(b as _);
                        let right = self.constant(aux as _);
                        statements.push(
                            ast::Assign::new(
                                vec![target.into()],
                                vec![ast::Binary::new(left.into(), right.into(), op).into()],
                            )
                            .into(),
                        );
                    }
                    OpCode::LOP_NOT | OpCode::LOP_MINUS | OpCode::LOP_LENGTH => {
                        let op = match op_code {
                            OpCode::LOP_NOT => ast::UnaryOperation::Not,
                            OpCode::LOP_MINUS => ast::UnaryOperation::Negate,
                            OpCode::LOP_LENGTH => ast::UnaryOperation::Length,
                            _ => unreachable!(),
                        };
                        let target = self.register(a as _);
                        let value = self.register(b as _);
                        statements.push(
                            ast::Assign::new(
                                vec![target.into()],
                                vec![ast::Unary::new(value.into(), op).into()],
                            )
                            .into(),
                        );
                    }
                    OpCode::LOP_RETURN => {
                        let values = if b != 0 {
                            (a..a + (b - 1) as u8)
                                .map(|r| self.register(r as _).into())
                                .collect()
                        } else {
                            let (tail, end) = top.take().unwrap();
                            (a..end)
                                .map(|r| self.register(r as _).into())
                                .chain(std::iter::once(tail))
                                .collect()
                        };
                        statements.push(ast::Return::new(values).into());
                    }
                    _ => unimplemented!("{:?}", instruction),
                },
                Instruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_LOADK => {
                        let constant = self.constant(d as _);
                        let target = self.register(a as _);
                        let statement =
                            ast::Assign::new(vec![target.into()], vec![constant.into()]);
                        statements.push(statement.into());
                    }
                    OpCode::LOP_LOADN => {
                        let target = self.register(a as _);
                        let statement = ast::Assign::new(
                            vec![target.into()],
                            vec![ast::Literal::Number(d as _).into()],
                        );
                        statements.push(statement.into());
                    }
                    OpCode::LOP_GETIMPORT => {
                        let target = self.register(a as _);
                        let import_len = (aux >> 30) & 3;
                        assert!(import_len == 1);
                        let import_name = self
                            .constant(((aux >> 20) & 1023) as usize)
                            .into_string()
                            .unwrap();
                        let global = ast::Global::new(import_name);
                        let assign = ast::Assign::new(vec![target.into()], vec![global.into()]);
                        statements.push(assign.into());
                        iter.next();
                    }
                    OpCode::LOP_JUMPIFNOT => {
                        let condition = self.register(a as _);
                        let statement = ast::If::new(
                            condition.into(),
                            ast::Block::default(),
                            ast::Block::default(),
                        );
                        edges.push((
                            self.block_to_node(block_start + index + 1),
                            BlockEdge::new(BranchType::Then),
                        ));
                        edges.push((
                            self.block_to_node(
                                ((block_start + index + 1) as isize + d as isize) as usize,
                            ),
                            BlockEdge::new(BranchType::Else),
                        ));
                        statements.push(statement.into());
                    }
                    OpCode::LOP_JUMPIF => {
                        let condition = self.register(a as _);
                        let statement = ast::If::new(
                            condition.into(),
                            ast::Block::default(),
                            ast::Block::default(),
                        );
                        edges.push((
                            self.block_to_node(
                                ((block_start + index + 1) as isize + d as isize) as usize,
                            ),
                            BlockEdge::new(BranchType::Then),
                        ));
                        edges.push((
                            self.block_to_node(block_start + index + 1),
                            BlockEdge::new(BranchType::Else),
                        ));
                        statements.push(statement.into());
                    }
                    OpCode::LOP_JUMPBACK => {
                        edges.push((
                            self.block_to_node(
                                ((block_start + index + 1) as isize + d as isize) as usize,
                            ),
                            BlockEdge::new(BranchType::Unconditional),
                        ));
                    }
                    _ => unimplemented!("{:?}", instruction),
                },
                _ => unimplemented!("{:?}", instruction),
            }
        }

        if edges.is_empty()
            && !Self::is_terminator(self.function_list[self.function].instructions[block_end])
        {
            edges.push((
                self.block_to_node(block_end + 1),
                BlockEdge::new(BranchType::Unconditional),
            ));
        }

        (statements, edges)
    }

    fn register(&mut self, index: usize) -> ast::RcLocal {
        self.register_map
            .entry(index)
            .or_insert_with(|| self.lifted_function.local_allocator.borrow_mut().allocate())
            .clone()
    }

    fn constant(&mut self, index: usize) -> ast::Literal {
        let converted_constant = match self.function_list[self.function]
            .constants
            .get(index)
            .unwrap()
        {
            BytecodeConstant::Nil => ast::Literal::Nil,
            BytecodeConstant::Boolean(v) => ast::Literal::Boolean(*v),
            BytecodeConstant::Number(v) => ast::Literal::Number(*v),
            BytecodeConstant::String(v) => {
                ast::Literal::String(self.string_table[*v - 1].chars().map(|x| x as _).collect())
            }
            _ => unimplemented!(),
        };
        self.constant_map
            .entry(index)
            .or_insert(converted_constant)
            .clone()
    }

    fn block_to_node(&self, insn_index: usize) -> NodeIndex {
        *self.blocks.get(&insn_index).unwrap()
    }

    fn is_terminator(instruction: Instruction) -> bool {
        match instruction {
            Instruction::BC { op_code, c, .. } => match op_code {
                OpCode::LOP_RETURN => true,
                OpCode::LOP_LOADB if c != 0 => true,
                _ => false,
            },
            Instruction::AD { op_code, .. } => match op_code {
                OpCode::LOP_JUMP
                | OpCode::LOP_JUMPBACK
                | OpCode::LOP_JUMPIF
                | OpCode::LOP_JUMPIFNOT
                | OpCode::LOP_JUMPIFEQ
                | OpCode::LOP_JUMPIFLE
                | OpCode::LOP_JUMPIFLT
                | OpCode::LOP_JUMPIFNOTEQ
                | OpCode::LOP_JUMPIFNOTLE
                | OpCode::LOP_JUMPIFNOTLT
                | OpCode::LOP_JUMPIFEQK
                | OpCode::LOP_JUMPIFNOTEQK => true,
                OpCode::LOP_FORNPREP
                | OpCode::LOP_FORNLOOP
                | OpCode::LOP_FORGPREP
                | OpCode::LOP_FORGLOOP
                | OpCode::LOP_FORGPREP_INEXT
                | OpCode::LOP_FORGLOOP_INEXT
                | OpCode::LOP_FORGPREP_NEXT
                | OpCode::LOP_FORGLOOP_NEXT => true,
                _ => false,
            },
            Instruction::E { op_code, .. } => matches!(op_code, OpCode::LOP_JUMPX),
        }
    }
}
