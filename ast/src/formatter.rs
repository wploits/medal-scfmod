use std::{
    borrow::{Borrow, Cow},
    collections::HashSet,
    fmt,
};

use itertools::Itertools;

use crate::{Assign, Block, LValue, RValue, Return, Statement};

pub enum IndentationMode {
    Spaces(u8),
    Tab,
}

impl IndentationMode {
    pub fn display(&self, indentation_level: usize) -> String {
        let string = self.to_string();

        string
            .chars()
            .cycle()
            .take(indentation_level * string.len())
            .collect()
    }
}

impl fmt::Display for IndentationMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Spaces(spaces) => Cow::Owned(" ".repeat(*spaces as usize)),
                Self::Tab => Cow::Borrowed("\u{09}"),
            }
        )
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

        for statement in &main.0 {
            formatter.format_statement(statement);
        }

        formatter.output
    }

    fn write(&mut self, iter: impl Iterator<Item = char>) {
        self.output.extend(iter);
    }

    fn indent(&mut self) {
        self.write(
            self.indentation_mode
                .display(self.indentation_level)
                .chars(),
        );
    }

    fn format_block(&mut self, block: &'a Block) {
        self.indentation_level += 1;

        for statement in &block.0[..block.len()
            - block
                .last()
                .map(|s| match s {
                    Statement::Return(Return { values }) => values.is_empty(),
                    _ => false,
                })
                .unwrap_or(false) as usize]
        {
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
                    if let (Some(name), RValue::Closure(function)) = (
                        match lvalue {
                            LValue::Local(local) => Some(local.0.to_string()),
                            LValue::Global(global) => Some(global.0.clone()),
                            _ => None,
                        },
                        rvalue,
                    ) {
                        self.write(
                            format!(
                                "function {}({})\n",
                                name,
                                function.parameters.iter().join(", ")
                            )
                            .chars(),
                        );
                        self.format_block(&function.body);
                        self.indent();
                        self.write("end".chars());
                    } else {
                        left.push(lvalue);
                        right.push(rvalue);
                    }
                }

                if let Some(RValue::Call(_)) = assign.right.last() {
                    let len = assign.left.len();

                    left.extend(&assign.left[len - 1..2 * len - 2])
                }

                if !(left.is_empty() || right.is_empty()) {
                    self.write(
                        format!("{} = {}", left.iter().join(", "), right.iter().join(", "),)
                            .chars(),
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
