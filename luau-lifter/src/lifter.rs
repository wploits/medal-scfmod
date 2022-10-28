
use std::{borrow::Cow, cell::RefCell, rc::Rc};
use std::collections::{HashMap, VecDeque};

use anyhow::Result;
use either::Either;
use rustc_hash::FxHashMap;

use ast;
use cfg::{block::Terminator, function::Function};
use graph::NodeId;
use super::{
    deserializer::{
        function::Function as BytecodeFunction,
        constant::Constant as BytecodeConstant
    },
    instruction::Instruction,
    op_code::OpCode
};

pub struct Lifter<'a> {
    function: usize,
    function_list: &'a Vec<BytecodeFunction>,
    string_table: &'a Vec<String>,
	blocks: FxHashMap<usize, NodeId>,
	lifted_function: Function<'a>,
	// TODO: make this a ref
	lifted_descendants: Vec<Rc<RefCell<Function<'a>>>>,
	closures: FxHashMap<usize, Rc<RefCell<Function<'a>>>>,
	register_map: FxHashMap<usize, Rc<ast::Local<'a>>>,
	constant_map: FxHashMap<usize, ast::Literal<'a>>,
	current_node: Option<NodeId>,
}

impl<'a> Lifter<'a> {
	pub fn new(
        f_list: &'a Vec<BytecodeFunction>,
        str_list: &'a Vec<String>,
        function_id: usize
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

	pub fn lift_function(mut self) -> Result<Function<'a>> {
		self.discover_blocks()?;

		let mut blocks = self.blocks.keys().cloned().collect::<Vec<_>>();

		blocks.sort_unstable();

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
			let parameter = self.lifted_function.local_allocator.allocate();
			self.lifted_function.parameters.push(parameter.clone());
			self.register_map.insert(i as usize, parameter);
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
		for (insn_index, insn) in self.function_list[self.function].instructions.iter().enumerate() {
			match insn {
                Instruction::BC { op_code, c, .. } => match op_code {
                    OpCode::LOP_LOADB if *c != 0 => {
                        self.blocks
                            .entry(insn_index.wrapping_add(*c as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    _ => {}
                }

                Instruction::AD { op_code, a, d, aux } => match op_code {
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
                    OpCode::LOP_FORNPREP | OpCode::LOP_FORGPREP | OpCode::LOP_FORGPREP_NEXT | OpCode::LOP_FORGPREP_INEXT => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_FORNLOOP => {
                        self.blocks
                            .entry(insn_index)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    OpCode::LOP_FORGLOOP | OpCode::LOP_FORGLOOP_NEXT | OpCode::LOP_FORGLOOP_INEXT => {
                        self.blocks
                            .entry(insn_index + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                        self.blocks
                            .entry(insn_index.wrapping_add(*d as usize) + 1)
                            .or_insert_with(|| self.lifted_function.new_block().unwrap());
                    }
                    _ => {}
                }

                Instruction::E { .. } => {}
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
		//let mut table_queue = Vec::new();
		//let mut table_elements = Vec::new();
		let mut terminator = None;

		for (index, instruction) in self.function_list[self.function]
            .instructions[block_start..=block_end]
			.iter()
			.enumerate()
		{
			let instruction_index = block_start + index;
			match *instruction {
                Instruction::BC { op_code, a, b, c, aux } => match op_code {
                    OpCode::LOP_NOP => {}
                    OpCode::LOP_BREAK => {}
                    OpCode::LOP_LOADNIL => unimplemented!(),
                    OpCode::LOP_LOADB => unimplemented!(),
                    OpCode::LOP_MOVE => unimplemented!(),
                    OpCode::LOP_GETGLOBAL => unimplemented!(),
                    OpCode::LOP_SETGLOBAL => {
                        statements.push(
                            ast::Assign {
                                left: vec![
                                    match self.constant(aux as usize).into() {
                                        ast::Literal::String(global) => Rc::new(ast::Local(global)).into(),
                                        _ => panic!("Invalid global"),
                                    }
                                ],
                                right: vec![self.register(a as usize).into()]
                            }.into()
                        );
                    }
                    OpCode::LOP_GETUPVAL => unimplemented!(),
                    OpCode::LOP_SETUPVAL => unimplemented!(),
                    OpCode::LOP_CLOSEUPVALS => unimplemented!(),
                    OpCode::LOP_GETTABLE => unimplemented!(),
                    OpCode::LOP_SETTABLE => unimplemented!(),
                    OpCode::LOP_GETTABLEKS => unimplemented!(),
                    OpCode::LOP_SETTABLEKS => unimplemented!(),
                    OpCode::LOP_GETTABLEN => unimplemented!(),
                    OpCode::LOP_SETTABLEN => unimplemented!(),
                    OpCode::LOP_NAMECALL => unimplemented!(),
                    OpCode::LOP_CALL => unimplemented!(),
                    OpCode::LOP_ADD => unimplemented!(),
                    OpCode::LOP_SUB => unimplemented!(),
                    OpCode::LOP_MUL => unimplemented!(),
                    OpCode::LOP_DIV => unimplemented!(),
                    OpCode::LOP_MOD => unimplemented!(),
                    OpCode::LOP_POW => unimplemented!(),
                    OpCode::LOP_ADDK => unimplemented!(),
                    OpCode::LOP_SUBK => unimplemented!(),
                    OpCode::LOP_MULK => unimplemented!(),
                    OpCode::LOP_DIVK => unimplemented!(),
                    OpCode::LOP_MODK => unimplemented!(),
                    OpCode::LOP_POWK => unimplemented!(),
                    OpCode::LOP_AND => unimplemented!(),
                    OpCode::LOP_OR => unimplemented!(),
                    OpCode::LOP_ANDK => unimplemented!(),
                    OpCode::LOP_ORK => unimplemented!(),
                    OpCode::LOP_CONCAT => unimplemented!(),
                    OpCode::LOP_NOT => unimplemented!(),
                    OpCode::LOP_MINUS => unimplemented!(),
                    OpCode::LOP_LENGTH => unimplemented!(),
                    OpCode::LOP_NEWTABLE => unimplemented!(),
                    OpCode::LOP_SETLIST => unimplemented!(),
                    OpCode::LOP_GETVARARGS => unimplemented!(),
                    OpCode::LOP_PREPVARARGS => {
                        // TODO: maybe do something with this? idk
                    },
                    OpCode::LOP_LOADKX => unimplemented!(),
                    OpCode::LOP_FASTCALL => {}
                    OpCode::LOP_CAPTURE => {}
                    OpCode::LOP_FASTCALL1 => {}
                    OpCode::LOP_FASTCALL2 => {}
                    OpCode::LOP_FASTCALL2K => {}
                    OpCode::LOP_RETURN => {
                        statements.push(ast::Return::default().into());
                        terminator = Some(Terminator::Return);
                    }
                    _ => unreachable!()
                }
                Instruction::AD { op_code, a, d, aux } => match op_code {
                    OpCode::LOP_LOADN => unimplemented!(),
                    OpCode::LOP_LOADK => {
                        statements.push(
                            ast::Assign {
                                left: vec![self.register(a as usize).into()],
                                right: vec![ast::RValue::Literal(self.constant(d as usize))]
                            }.into()
                        );
                    }
                    OpCode::LOP_GETIMPORT => unimplemented!(),
                    OpCode::LOP_NEWCLOSURE => unimplemented!(),
                    OpCode::LOP_JUMP => unimplemented!(),
                    OpCode::LOP_JUMPBACK => unimplemented!(),
                    OpCode::LOP_JUMPIF => unimplemented!(),
                    OpCode::LOP_JUMPIFNOT => unimplemented!(),
                    OpCode::LOP_JUMPIFEQ => unimplemented!(),
                    OpCode::LOP_JUMPIFLE => unimplemented!(),
                    OpCode::LOP_JUMPIFLT => unimplemented!(),
                    OpCode::LOP_JUMPIFNOTEQ => unimplemented!(),
                    OpCode::LOP_JUMPIFNOTLE => unimplemented!(),
                    OpCode::LOP_JUMPIFNOTLT => unimplemented!(),
                    OpCode::LOP_DUPTABLE => unimplemented!(),
                    OpCode::LOP_FORNPREP => unimplemented!(),
                    OpCode::LOP_FORNLOOP => unimplemented!(),
                    OpCode::LOP_FORGLOOP => unimplemented!(),
                    OpCode::LOP_FORGPREP_INEXT => unimplemented!(),
                    OpCode::LOP_FORGLOOP_INEXT => unimplemented!(),
                    OpCode::LOP_FORGPREP_NEXT => unimplemented!(),
                    OpCode::LOP_FORGLOOP_NEXT => unimplemented!(),
                    OpCode::LOP_DUPCLOSURE => unimplemented!(),
                    OpCode::LOP_JUMPIFEQK => unimplemented!(),
                    OpCode::LOP_JUMPIFNOTEQK => unimplemented!(),
                    OpCode::LOP_FORGPREP => unimplemented!(),
                    _ => unreachable!()
                }
                Instruction::E { op_code, e } => match op_code {
                    // there is technically only one actual e opcode that is used
                    // LOP_COVERAGE should **never** be generated outside of studio bytecode
                    OpCode::LOP_JUMPX => unimplemented!(),
                    _ => unreachable!()
                }
            }
		}
		(
			statements,
			terminator.unwrap_or_else(|| Terminator::Jump(self.block_to_node(block_end + 1))),
		)
	}

	fn register(&mut self, index: usize) -> Rc<ast::Local<'a>> {
		self.register_map
			.entry(index)
			.or_insert_with(|| self.lifted_function.local_allocator.allocate())
			.clone()
	}

	fn constant(&mut self, index: usize) -> ast::Literal<'a> {
		let converted_constant = match self.function_list[self.function]
            .constants.get(index).unwrap() {
			BytecodeConstant::Nil => ast::Literal::Nil,
			BytecodeConstant::Boolean(v) => ast::Literal::Boolean(*v),
			BytecodeConstant::Number(v) => ast::Literal::Number(*v),
			BytecodeConstant::String(v) => 
                ast::Literal::String(Cow::Owned(self.string_table[*v - 1].clone())),
            _ => unimplemented!()
		};
		self.constant_map
			.entry(index)
			.or_insert(converted_constant)
			.clone()
	}

	fn block_to_node(&self, insn_index: usize) -> NodeId {
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
