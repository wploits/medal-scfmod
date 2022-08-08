use std::{borrow::Cow, fmt};
use std::borrow::Borrow;
use std::collections::HashSet;

use itertools::Itertools;

use crate::{Assign, Block, LValue, Return, RValue, Statement};

pub enum IndentationMode {
	Spaces(u8),
	Tab,
}

impl IndentationMode {
	pub fn display(&self, indentation_level: usize) -> String {
		let string = self.to_string();

		string.chars().cycle().take(indentation_level * string.len()).collect()
	}
}

impl fmt::Display for IndentationMode {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Spaces(spaces) => Cow::Owned(vec![' '; *spaces as usize].iter().join("")),
			Self::Tab => Cow::Borrowed("\u{09}"),
		})
	}
}

impl Default for IndentationMode {
	fn default() -> Self {
		Self::Tab
	}
}

pub struct Formatter<'a> {
	indentation_level: usize,
	indentation_mode: IndentationMode,
	locals_stack: Vec<HashSet<&'a String>>,
	output: String,
}

impl<'a> Formatter<'a> {
	pub fn format(main: &Block, indentation_mode: IndentationMode) -> String {
		let mut formatter = Self {
			indentation_level: 0,
			indentation_mode,
			locals_stack: vec![HashSet::new()],
			output: String::new(),
		};

		for statement in &main[..main.len() - 1] {
			formatter.format_statement(statement);
		}

		formatter.output
	}

	fn write(&mut self, iter: impl Iterator<Item=char>) {
		self.output.extend(iter);
	}

	fn indent(&mut self) {
		self.write(self.indentation_mode.display(self.indentation_level).chars());
	}

	fn new_scope(&mut self) {
		self.locals_stack.push(HashSet::new());
	}

	fn pop_scope(&mut self) {
		self.locals_stack.pop();
	}

	fn scope(&mut self, local: &'a String) -> bool {
		let locals = self.locals_stack.last_mut().unwrap();

		if locals.contains(local) {
			true
		} else {
			locals.insert(local);

			false
		}
	}


	fn format_block(&mut self, block: &'a Block) {
		self.indentation_level += 1;

		for statement in &block.0[..block.len() - block
			.last()
			.map(|s| match s {
				Statement::Return(Return { values }) => values.is_empty(),
				_ => false,
			}).unwrap_or(false) as usize] {
			self.format_statement(statement);
		}

		self.indentation_level -= 1;
	}

	fn format_statement(&mut self, statement: &'a Statement) {
		self.indent();

		match statement {
			Statement::Assign(assign) => {
				let mut left = Vec::new();
				let mut right = Vec::new();

				for (lvalue, rvalue) in assign.left.iter().zip(assign.right.iter()) {
					if let LValue::Local(local) = lvalue {
						if !self.scope(&local.0.0) {
							self.write("local ".chars());
						}
					}

					if let (Some(name), RValue::Closure(function)) = (match lvalue {
						LValue::Local(local) => Some(local.0.to_string()),
						LValue::Global(global) => Some(global.0.clone()),
						_ => None,
					}, rvalue) {
						self.write(format!("function {}({})\n", name, function.parameters.iter().join(", ")).chars());
						self.new_scope();
						self.format_block(&function.body);
						self.pop_scope();
						self.indent();
						self.write("end".chars());
					} else {
						left.push(lvalue);
						right.push(rvalue);
					}
				}

				if !(left.is_empty() || right.is_empty()) {
					self.write(
						format!(
							"{} = {}",
							left.iter().join(", "),
							right.iter().join(", "),
						).chars()
					);
				}
			}
			Statement::If(r#if) => {
				if let Some(b) = &r#if.then_block {
					self.format_block(b);
				}

				if let Some(b) = &r#if.else_block {
					self.format_block(b);
				}
			}
			Statement::While(r#while) => self.format_block(&r#while.block),
			_ => self.write(statement.to_string().chars()),
		}

		self.output.push('\n');
	}
}
