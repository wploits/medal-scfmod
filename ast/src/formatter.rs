use std::{borrow::Cow, fmt, iter, io};

use itertools::Itertools;

use crate::{Assign, Block, Call, If, Index, LValue, Literal, RValue, Return, Select, Statement};

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

pub(crate) fn format_arg_list(list: &[RValue]) -> String {
    let mut s = String::new();
    for (index, rvalue) in list.iter().enumerate() {
        if index + 1 == list.len() {
            if matches!(rvalue, RValue::Select(_)) {
                s += &format!("({})", rvalue);
            } else {
                s += &rvalue.to_string();
            }
        } else {
            s += &format!("{}, ", rvalue);
        }
    }
    s
}

pub struct Formatter<'a, W: io::Write> {
    indentation_level: usize,
    indentation_mode: IndentationMode,
    output: &'a mut W,
}

impl<'a, W: io::Write> Formatter<'a, W> {
    pub fn format(main: &Block, output: &'a mut W, indentation_mode: IndentationMode) -> io::Result<()> {
        let mut formatter = Self {
            indentation_level: 0,
            indentation_mode,
            output,
        };
        formatter.format_block_no_indent(main)
    }

    fn indent(&mut self) -> io::Result<()> {
        write!(self.output, "{}",
            // TODO: make display return an iterator
            self.indentation_mode
                .display(self.indentation_level)
        )
    }

    // (function() end)()
    // (function() end)[1]
    fn should_wrap_left_rvalue(value: &RValue) -> bool {
        !matches!(
            value,
            RValue::Local(_)
                | RValue::Global(_)
                | RValue::Index(_)
                | RValue::Select(Select::Call(_))
        )
    }

    fn format_block(&mut self, block: &Block) -> io::Result<()> {
        self.indentation_level += 1;
        self.format_block_no_indent(block)?;
        self.indentation_level -= 1;
        Ok(())
    }

    fn format_block_no_indent(&mut self, block: &Block) -> io::Result<()> {
        for (i, statement) in block.iter().enumerate() {
            if i != 0 {
                writeln!(self.output)?;
            }
            self.format_statement(statement)?;
            if let Some(next_statement) =
                block.iter().skip(i + 1).find(|s| s.as_comment().is_none())
            {
                fn is_ambiguous(r: &RValue) -> bool {
                    match r {
                        RValue::Local(_)
                        | RValue::Global(_)
                        | RValue::Index(_)
                        | RValue::Call(_) => true,
                        RValue::Binary(binary) => is_ambiguous(&binary.right),
                        _ => false,
                    }
                }

                let disambiguate = match statement {
                    Statement::Call(_) => true,
                    Statement::Repeat(repeat) => is_ambiguous(&repeat.condition),
                    Statement::Assign(Assign { right: list, .. })
                    | Statement::Return(Return { values: list }) => {
                        if let Some(last) = list.last() {
                            is_ambiguous(last)
                        } else {
                            false
                        }
                    }
                    Statement::Goto(_) | Statement::Continue(_) | Statement::Break(_) => true,
                    _ => false,
                };
                let disambiguate = disambiguate
                    && match next_statement {
                        Statement::Assign(Assign {
                            left,
                            prefix: false,
                            ..
                        }) => {
                            if let Some(index) = left[0].as_index() {
                                Self::should_wrap_left_rvalue(&index.left)
                            } else {
                                false
                            }
                        }
                        Statement::Call(call) => Self::should_wrap_left_rvalue(&call.value),
                        Statement::Comment(_) => unimplemented!(),
                        _ => false,
                    };
                if disambiguate {
                    write!(self.output, ";")?;
                }
            }
        }
        Ok(())
    }

    fn format_lvalue(&mut self, lvalue: &LValue) -> io::Result<()> {
        match lvalue {
            LValue::Index(index) => self.format_index(index),
            _ => write!(self.output, "{}", lvalue)
        }
    }

    fn format_rvalue(&mut self, rvalue: &RValue) -> io::Result<()> {
        match rvalue {
            RValue::Select(Select::Call(call)) | RValue::Call(call) => self.format_call(call)?,
            RValue::Table(table) => {
                write!(self.output, "{{")?;
                for (index, (key, value)) in table.0.iter().enumerate() {
                    let is_last = index + 1 == table.0.len();
                    if is_last && key.is_none() {
                        let wrap = matches!(value, RValue::Select(_));
                        if wrap {
                            write!(self.output, "(")?;
                        }
                        self.format_rvalue(value)?;
                        if wrap {
                            write!(self.output, ")")?;
                        }
                    } else {
                        if let Some(key) = key {
                            write!(self.output, "[")?;
                            self.format_rvalue(key)?;
                            write!(self.output, "] = ")?;
                        }
                        self.format_rvalue(value)?;
                        if !is_last {
                            write!(self.output, ", ")?;
                        }
                    }
                }
                write!(self.output, "}}")?;
            }
            RValue::Index(index) => self.format_index(index)?,
            RValue::Unary(unary) => {
                write!(self.output, "{}", unary.operation)?;
                let wrap =
                    unary.precedence() > unary.value.precedence() && unary.value.precedence() != 0;
                if wrap {
                    write!(self.output, "(")?;
                }
                self.format_rvalue(&unary.value)?;
                if wrap {
                    write!(self.output, ")")?;
                }
            }
            RValue::Binary(binary) => {
                let parentheses = |f: &mut Self, rvalue: &RValue| -> io::Result<()> {
                    let wrap =
                        binary.precedence() > rvalue.precedence() && rvalue.precedence() != 0;
                    if wrap {
                        write!(f.output, "(")?;
                    }
                    f.format_rvalue(rvalue)?;
                    if wrap {
                        write!(f.output, ")")?;
                    }
                    Ok(())
                };

                parentheses(self, &binary.left)?;
                write!(self.output, " {} ",
                    binary.operation
                )?;
                parentheses(self, &binary.right)?;
            }
            RValue::Closure(closure) => {
                if closure.is_variadic {
                    write!(self.output,
                            "function({})",
                            closure
                                .parameters
                                .iter()
                                .map(|x| x.to_string())
                                .chain(std::iter::once("...".into()))
                                .join(", ")
                    )?;
                } else {
                    write!(self.output, "function({})", closure.parameters.iter().join(", "))?;
                }
                if !closure.body.is_empty() {
                    // TODO: output.push?
                    writeln!(self.output)?;
                    self.format_block(&closure.body)?;
                    writeln!(self.output)?;
                    self.indent()?;
                }
                write!(self.output, "end")?;
            }
            _ => write!(self.output, "{}", rvalue)?,
        }
        Ok(())
    }

    fn format_arg_list(&mut self, list: &[RValue]) -> io::Result<()> {
        for (index, rvalue) in list.iter().enumerate() {
            if index + 1 == list.len() {
                let wrap = matches!(rvalue, RValue::Select(_));
                if wrap {
                    write!(self.output, "(")?;
                }
                self.format_rvalue(rvalue)?;
                if wrap {
                    write!(self.output, ")")?;
                }
            } else {
                self.format_rvalue(rvalue)?;
                write!(self.output, ", ")?;
            }
        }
        Ok(())
    }

    fn format_index(&mut self, index: &Index) -> io::Result<()> {
        let wrap = Self::should_wrap_left_rvalue(&index.left);
        if wrap {
            write!(self.output, "(")?;
        }
        self.format_rvalue(&index.left)?;
        if wrap {
            write!(self.output, ")")?;
        }

        match index.right.as_ref() {
            RValue::Literal(super::Literal::String(field))
                if field.is_ascii()
                    && field.chars().enumerate().all(|(i, c)| {
                        (i != 0 && c.is_ascii_digit()) || c.is_ascii_alphabetic() || c == '_'
                    }) =>
            {
                write!(self.output, ".{}", field)
            }
            _ => {
                write!(self.output, "[")?;
                self.format_rvalue(&index.right)?;
                write!(self.output, "]")
            }
        }
    }

    fn format_call(&mut self, call: &Call) -> io::Result<()> {
        let wrap = Self::should_wrap_left_rvalue(&call.value);
        if wrap {
            write!(self.output, "(")?;
        }
        self.format_rvalue(&call.value)?;
        if wrap {
            write!(self.output, ")")?;
        }

        write!(self.output, "(")?;
        self.format_arg_list(&call.arguments)?;
        write!(self.output, ")")
    }

    fn format_if(&mut self, r#if: &If) -> io::Result<()> {
        write!(self.output, "if ")?;

        self.format_rvalue(&r#if.condition)?;

        writeln!(self.output, " then")?;

        if let Some(b) = &r#if.then_block {
            self.format_block(b)?;
            writeln!(self.output)?;
        }

        if let Some(b) = &r#if.else_block {
            assert!(r#if.then_block.is_some());
            self.indent()?;
            if b.len() == 1
                && let Some(else_if) = b[0].as_if()
            {
                write!(self.output, "else")?;
                return self.format_if(else_if);
            }
            writeln!(self.output, "else")?;
            self.format_block(b)?;
            writeln!(self.output)?;
        }

        self.indent()?;
        write!(self.output, "end")
    }

    fn format_statement(&mut self, statement: &Statement) -> io::Result<()> {
        self.indent()?;

        match statement {
            Statement::Assign(assign) => {
                /*let mut left = Vec::new();
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
                        write!(self.output, "\n")?;
                        self.indent();
                        write!(self.output, "end")?;
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

                left = assign.left.clone();
                right = assign.right.clone();*/

                if assign.prefix {
                    write!(self.output, "local ")?;
                }

                /*if !(left.is_empty() || right.is_empty()) {
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
                }*/

                for (i, lvalue) in assign.left.iter().enumerate() {
                    if i != 0 {
                        write!(self.output, ", ")?;
                    }
                    self.format_lvalue(lvalue)?;
                }

                write!(self.output, " = ")?;

                // TODO: move to format_rvalue_list function
                for (i, rvalue) in assign.right.iter().enumerate() {
                    if i != 0 {
                        write!(self.output, ", ")?;
                    }
                    self.format_rvalue(rvalue)?;
                }
            }
            Statement::If(r#if) => self.format_if(r#if)?,
            Statement::While(r#while) => {
                write!(self.output, "while ")?;

                self.format_rvalue(&r#while.condition)?;

                writeln!(self.output, " do")?;

                self.format_block(&r#while.block)?;
                writeln!(self.output)?;
                self.indent()?;
                write!(self.output, "end")?;
            }
            Statement::Repeat(repeat) => {
                writeln!(self.output, "repeat")?;
                self.format_block(&repeat.block)?;
                writeln!(self.output)?;
                self.indent()?;

                write!(self.output, "until ")?;

                self.format_rvalue(&repeat.condition)?;
            }
            Statement::NumericFor(numeric_for) => {
                write!(self.output, "for {} = ", numeric_for.counter)?;
                self.format_rvalue(&numeric_for.initial)?;
                write!(self.output, ", ")?;
                self.format_rvalue(&numeric_for.limit)?;
                let skip_step = if let RValue::Literal(Literal::Number(n)) = numeric_for.step {
                    n == 1.0
                } else {
                    false
                };
                if !skip_step {
                    write!(self.output, ", ")?;
                    self.format_rvalue(&numeric_for.step)?;
                }
                writeln!(self.output, " do")?;
                self.format_block(&numeric_for.block)?;
                writeln!(self.output)?;
                self.indent()?;
                write!(self.output, "end")?;
            }
            Statement::GenericFor(generic_for) => {
                write!(self.output, "for {} in ", generic_for.res_locals.iter().join(", "))?;
                for (i, rvalue) in generic_for.right.iter().enumerate() {
                    if i != 0 {
                        write!(self.output, ", ")?;
                    }
                    self.format_rvalue(rvalue)?;
                }
                writeln!(self.output, " do")?;
                self.format_block(&generic_for.block)?;
                writeln!(self.output)?;
                self.indent()?;
                write!(self.output, "end")?;
            }
            Statement::Call(c) => self.format_call(c)?,
            Statement::Return(r#return) => {
                write!(self.output, "return")?;
                for (i, rvalue) in r#return.values.iter().enumerate() {
                    if i == 0 {
                        write!(self.output, " ")?;
                    } else {
                        write!(self.output, ", ")?;
                    }
                    self.format_rvalue(rvalue)?;
                }
            }
            _ => write!(self.output, "{}", statement)?,
        }
        Ok(())
    }
}
