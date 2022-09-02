use std::{borrow::Cow, fmt};

use itertools::Itertools;

use crate::{Block, LValue, RValue, Return, Statement, Type};

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

pub struct Formatter {
    indentation_level: usize,
    indentation_mode: IndentationMode,
    output: String,
}

impl Formatter {
    pub fn format(main: &Block, indentation_mode: IndentationMode) -> String {
        let mut formatter = Self {
            indentation_level: 0,
            indentation_mode,
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

    fn format_block(&mut self, block: &Block) {
        self.indentation_level += 1;

        for statement in &block.0[..block.len()
            - block
                .last()
                .map(|s| match s {
                    Statement::Return(Return { values }) => values.is_empty(),
                    _ => false,
                })
                .unwrap_or_default() as usize]
        {
            self.format_statement(statement);
        }

        self.indentation_level -= 1;
    }

    fn format_statement(&mut self, statement: &Statement) {
        self.indent();

        match statement {
            Statement::Assign(assign) => {
                let mut left = Vec::new();
                let mut right = Vec::new();

                for ((lvalue, annotation), rvalue) in assign.left.iter().zip(assign.right.iter()) {
                    if let (Some(name), RValue::Closure(function)) = (
                        match lvalue {
                            LValue::Local(local) => Some(local.0.to_string()),
                            LValue::Global(global) => Some(global.0.clone()),
                            _ => None,
                        },
                        rvalue,
                    ) {
                        let (parameters_types, return_values_types) =
                            match annotation.as_ref().unwrap() {
                                Type::Function(p, r) => (p, r),
                                _ => unreachable!(),
                            };

                        self.write(
                            format!(
                                "function {}({}){}\n",
                                name,
                                function
                                    .parameters
                                    .iter()
                                    .zip(parameters_types.iter())
                                    .map(|(l, t)| format!("{}: {}", l, t))
                                    .join(", "),
                                if return_values_types.is_empty() {
                                    String::new()
                                } else if return_values_types.len() == 1 {
                                    format!(": {}", return_values_types[0])
                                } else {
                                    format!(": ({})", return_values_types.iter().join(", "))
                                }
                            )
                            .chars(),
                        );
                        self.format_block(&function.body);
                        self.indent();
                        self.write("end".chars());
                    } else {
                        left.push((lvalue, annotation));
                        right.push(rvalue);
                    }
                }

                if let Some(RValue::Call(_)) = assign.right.last() {
                    let len = assign.left.len();

                    left.extend(
                        assign.left[len - 1..2 * len - 2]
                            .iter()
                            .map(|(a, b)| (a, b)),
                    )
                }

                if !(left.is_empty() || right.is_empty()) {
                    self.write(
                        format!(
                            "{} = {}",
                            left.iter()
                                .map(|(lvalue, annotation)| {
                                    format!(
                                        "{}{}",
                                        lvalue,
                                        annotation
                                            .as_ref()
                                            .map(|t| format!(": {}", t))
                                            .unwrap_or_default(),
                                    )
                                })
                                .join(", "),
                            right.iter().join(", ")
                        )
                        .chars(),
                    );
                }
            }
            Statement::If(r#if) => {
                self.write(format!("if {} then\n", r#if.condition).chars());

                if let Some(b) = &r#if.then_block {
                    self.format_block(b);
                }

                if let Some(b) = &r#if.else_block {
                    self.indent();
                    self.write("else\n".chars());
                    self.format_block(b);
                }

                self.indent();
                self.write("end".chars());
            }
            Statement::While(r#while) => {
                self.write(format!("while {} then\n", r#while.condition).chars());
                self.format_block(&r#while.block);
                self.indent();
                self.write("end".chars());
            }
            _ => self.write(statement.to_string().chars()),
        }

        self.output.push('\n');
    }
}
