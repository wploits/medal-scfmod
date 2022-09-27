use std::{borrow::Cow, fmt, iter};

use itertools::Itertools;

use crate::{
    Assign, Block, Call, If, Index, LValue, Literal, RValue, Return, Select, Statement,
};

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
        formatter.format_block_no_indent(main);
        formatter.output.trim_end().to_string()
    }

    fn write(&mut self, iter: impl Iterator<Item = char>) {
        self.output.extend(iter);
    }

    fn indent(&mut self) {
        self.write(
            // TODO: make display return an iterator
            self.indentation_mode
                .display(self.indentation_level)
                .chars(),
        );
    }

    // (function() end)()
    // (function() end)[1]
    fn should_wrap_left_rvalue(value: &RValue) -> bool {
        !matches!(
            value,
            RValue::Local(_) | RValue::Global(_) | RValue::Index(_)
        )
    }

    fn format_block(&mut self, block: &Block) {
        self.indentation_level += 1;
        self.format_block_no_indent(block);
        self.indentation_level -= 1;
    }

    fn format_block_no_indent(&mut self, block: &Block) {
        for (i, statement) in block.iter().enumerate() {
            self.format_statement(statement);
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
                        is_ambiguous(list.last().unwrap())
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
                    self.write(iter::once(';'));
                }
            }
            self.write(iter::once('\n'));
        }
    }

    fn format_lvalue(&mut self, lvalue: &LValue) {
        match lvalue {
            LValue::Index(index) => self.format_index(index),
            _ => self.write(lvalue.to_string().chars()),
        }
    }

    fn format_rvalue(&mut self, rvalue: &RValue) {
        match rvalue {
            RValue::Select(Select::Call(call)) | RValue::Call(call) => self.format_call(call),
            RValue::Table(table) => {
                self.write("{".chars());
                self.format_arg_list(&table.0);
                self.write("}".chars());
            }
            RValue::Index(index) => self.format_index(index),
            RValue::Unary(unary) => {
                self.write(unary.operation.to_string().chars());
                let wrap =
                    unary.precedence() > unary.value.precedence() && unary.value.precedence() != 0;
                if wrap {
                    self.write("(".chars());
                }
                self.format_rvalue(&unary.value);
                if wrap {
                    self.write(")".chars());
                }
            }
            RValue::Binary(binary) => {
                let parentheses = |f: &mut Self, rvalue: &RValue| {
                    let wrap =
                        binary.precedence() > rvalue.precedence() && rvalue.precedence() != 0;
                    if wrap {
                        f.write("(".chars());
                    }
                    f.format_rvalue(rvalue);
                    if wrap {
                        f.write(")".chars());
                    }
                };

                parentheses(self, &binary.left);
                self.write(
                    iter::once(' ')
                        .chain(binary.operation.to_string().chars())
                        .chain(iter::once(' ')),
                );
                parentheses(self, &binary.right);
            }
            RValue::Closure(closure) => {
                if closure.is_variadic {
                    self.write(
                        format!(
                            "function({})",
                            closure
                                .parameters
                                .iter()
                                .map(|x| x.to_string())
                                .chain(std::iter::once("...".into()))
                                .join(", ")
                        )
                        .chars(),
                    );
                } else {
                    self.write(
                        format!("function({})", closure.parameters.iter().join(", ")).chars(),
                    );
                }
                if !closure.body.is_empty() {
                    // TODO: output.push?
                    self.write(iter::once('\n'));
                    self.format_block(&closure.body);
                    self.indent();
                }
                self.write("end".chars());
            }
            _ => self.write(rvalue.to_string().chars()),
        }
    }

    fn format_arg_list(&mut self, list: &[RValue]) {
        for (index, rvalue) in list.iter().enumerate() {
            if index + 1 == list.len() {
                let wrap = matches!(rvalue, RValue::Select(_));
                if wrap {
                    self.write("(".chars());
                }
                self.format_rvalue(rvalue);
                if wrap {
                    self.write(")".chars());
                }
            } else {
                self.format_rvalue(rvalue);
                self.write(", ".chars());
            }
        }
    }

    fn format_index(&mut self, index: &Index) {
        let wrap = Self::should_wrap_left_rvalue(&index.left);
        if wrap {
            self.write("(".chars());
        }
        self.format_rvalue(&index.left);
        if wrap {
            self.write(")".chars());
        }

        match index.right.as_ref() {
            RValue::Literal(super::Literal::String(field))
                if field.is_ascii()
                    && field.chars().enumerate().all(|(i, c)| {
                        (i != 0 && c.is_ascii_digit()) || c.is_ascii_alphabetic() || c == '_'
                    }) =>
            {
                self.write(format!(".{}", field).chars())
            }
            _ => {
                self.write("[".chars());
                self.format_rvalue(&index.right);
                self.write("]".chars());
            }
        }
    }

    fn format_call(&mut self, call: &Call) {
        let wrap = Self::should_wrap_left_rvalue(&call.value);
        if wrap {
            self.write("(".chars());
        }
        self.format_rvalue(&call.value);
        if wrap {
            self.write(")".chars());
        }

        self.write("(".chars());
        self.format_arg_list(&call.arguments);
        self.write(")".chars());
    }

    fn format_if(&mut self, r#if: &If) {
        self.write("if ".chars());

        self.format_rvalue(&r#if.condition);

        self.write(" then\n".chars());

        if let Some(b) = &r#if.then_block {
            self.format_block(b);
        }

        if let Some(b) = &r#if.else_block {
            self.indent();
            if b.len() == 1
                && let Some(else_if) = b[0].as_if()
            {
                self.write("else".chars());
                self.format_if(else_if);
                return;
            }
            self.write("else\n".chars());
            self.format_block(b);
        }

        self.indent();
        self.write("end".chars());
    }

    fn format_statement(&mut self, statement: &Statement) {
        self.indent();

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

                left = assign.left.clone();
                right = assign.right.clone();*/

                if assign.prefix {
                    self.write("local ".chars());
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
                        self.write(", ".chars());
                    }
                    self.format_lvalue(lvalue);
                }

                self.write(" = ".chars());

                // TODO: move to format_rvalue_list function
                for (i, rvalue) in assign.right.iter().enumerate() {
                    if i != 0 {
                        self.write(", ".chars());
                    }
                    self.format_rvalue(rvalue);
                }
            }
            Statement::If(r#if) => self.format_if(r#if),
            Statement::While(r#while) => {
                self.write("while ".chars());

                self.format_rvalue(&r#while.condition);

                self.write(" do\n".chars());

                self.format_block(&r#while.block);
                self.indent();
                self.write("end".chars());
            }
            Statement::Repeat(repeat) => {
                self.write("repeat\n".chars());
                self.format_block(&repeat.block);
                self.indent();

                self.write("until ".chars());

                self.format_rvalue(&repeat.condition);
            }
            Statement::NumericFor(numeric_for) => {
                self.write(format!("for {} = ", numeric_for.counter).chars());
                self.format_rvalue(&numeric_for.initial);
                self.write(", ".chars());
                self.format_rvalue(&numeric_for.limit);
                let skip_step = if let RValue::Literal(Literal::Number(n)) = numeric_for.step {
                    n == 1.0
                } else {
                    false
                };
                if !skip_step {
                    self.write(", ".chars());
                    self.format_rvalue(&numeric_for.step);
                }
                self.write(" do\n".chars());
                self.format_block(&numeric_for.block);
                self.indent();
                self.write("end".chars());
            }
            Statement::GenericFor(generic_for) => {
                self.write(
                    format!("for {} in ", generic_for.res_locals.iter().join(", "),).chars(),
                );
                for (i, rvalue) in generic_for.right.iter().enumerate() {
                    if i != 0 {
                        self.write(", ".chars());
                    }
                    self.format_rvalue(rvalue);
                }
                self.write(" do\n".chars());
                self.format_block(&generic_for.block);
                self.indent();
                self.write("end".chars());
            }
            Statement::Call(c) => self.format_call(c),
            Statement::Return(r#return) => {
                self.write("return".chars());
                for (i, rvalue) in r#return.values.iter().enumerate() {
                    if i == 0 {
                        self.write(iter::once(' '));
                    } else {
                        self.write(", ".chars());
                    }
                    self.format_rvalue(rvalue);
                }
            }
            _ => self.write(statement.to_string().chars()),
        }
    }
}
