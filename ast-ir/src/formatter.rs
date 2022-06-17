use super::{BinaryOp, Block, Call, Expr, Function, Lit, Stat, UnaryOp};

#[derive(Debug)]
struct Formatter {
    output: String,
    indentation_level: usize,
    is_indented: bool,
}

impl Formatter {
    fn new() -> Self {
        Formatter {
            output: String::new(),
            indentation_level: 0,
            is_indented: false,
        }
    }

    fn print(&mut self, text: &str) {
        if !self.is_indented {
            self.output += &"    ".repeat(self.indentation_level);
            self.is_indented = true;
        }
        self.output += text;
    }

    fn newline(&mut self) {
        self.print("\n");
        self.is_indented = false;
    }

    fn indent(&mut self) {
        self.indentation_level += 1;
    }

    fn unindent(&mut self) {
        self.indentation_level -= 1;
    }

    fn format_lit(&mut self, lit: &Lit) {
        match lit {
            Lit::Nil => self.print("nil"),
            Lit::Boolean(b) => self.print(&b.to_string()),
            Lit::Number(n) => self.print(&n.to_string()),
            // TODO: we could optimize this by removing format and appending '"' directly to the output, if needed
            Lit::String(s) => self.print(&format!("\"{}\"", &s.to_string())),
        }
    }

    fn format_binary_op(&mut self, op: &BinaryOp) {
        match op {
            BinaryOp::Add => self.print("+"),
            BinaryOp::Sub => self.print("-"),
            BinaryOp::Mul => self.print("*"),
            BinaryOp::Div => self.print("/"),
            BinaryOp::Mod => self.print("%"),
            BinaryOp::Pow => self.print("^"),
            BinaryOp::LesserThan => self.print("<"),
            BinaryOp::LesserOrEqual => self.print("<="),
            BinaryOp::Equal => self.print("=="),
            BinaryOp::LogicalAnd => self.print("and"),
            BinaryOp::LogicalOr => self.print("or"),
            BinaryOp::Concat => self.print(".."),
            _ => unimplemented!(),
        };
    }

    fn format_unary_op(&mut self, op: &UnaryOp) {
        match op {
            UnaryOp::Minus => self.print("-"),
            UnaryOp::LogicalNot => self.print("not "),
            UnaryOp::Len => self.print("#"),
        };
    }

    fn format_call(&mut self, call: &Call) {
        self.format_expression(&call.value);
        self.print("(");
        for (argument_index, argument) in call.arguments.iter().enumerate() {
            self.format_expression(argument);
            if argument_index + 1 != call.arguments.len() {
                self.print(", ");
            }
        }
        self.print(")");
    }

    fn format_expression(&mut self, expression: &Expr) {
        match expression {
            Expr::Local(local_expr) => {
                self.print(&local_expr.local.name);
            }
            Expr::Global(global_expr) => {
                self.print(&global_expr.name);
            }
            Expr::Lit(lit_expr) => {
                self.format_lit(&lit_expr.lit);
            }
            Expr::Binary(binary_expr) => {
                self.format_expression(&binary_expr.lhs);
                self.print(" ");
                self.format_binary_op(&binary_expr.op);
                self.print(" ");
                self.format_expression(&binary_expr.rhs);
            }
            Expr::Unary(unary_expr) => {
                self.format_unary_op(&unary_expr.op);
                self.format_expression(&unary_expr.expr);
            }
            Expr::Index(index_expr) => {
                self.format_expression(&index_expr.expr);
                for index in &index_expr.indices {
                    self.print("[");
                    self.format_expression(index);
                    self.print("]");
                }
            }
            Expr::Table(table_expr) => {
                self.print("{");
                for (_key, _value) in &table_expr.pairs {
                    // TODO: table formatting
                    todo!();
                }
                self.print("}")
            }
            Expr::Call(call) => self.format_call(call),
            Expr::Varargs(_) => {
                self.print("...");
            }
            _ => {}
        }
    }

    fn format_block(&mut self, block: &Block) {
        for statement in &block.statements {
            match statement {
                Stat::Assign(assign) => {
                    if assign.local_prefix {
                        self.print("local ");
                    }
                    for (var_index, var) in assign.vars.iter().enumerate() {
                        self.format_expression(var);
                        if var_index + 1 != assign.vars.len() {
                            self.print(", ");
                        }
                    }
                    self.print(" = ");
                    self.format_expression(&assign.values[0]);
                }
                Stat::While(while_stat) => {
                    self.print("while ");
                    self.format_expression(&while_stat.condition);
                    self.print(" do");
                    self.indent();
                    self.newline();
                    self.format_block(&while_stat.body);
                    self.unindent();
                    self.print("end");
                }
                Stat::NumericFor(for_stat) => {
                    self.print("for ");
                    self.print(&for_stat.var.name);
                    self.print(" = ");
                    self.format_expression(&for_stat.from);
                    self.print(", ");
                    self.format_expression(&for_stat.to);
                    if let Some(step_expr) = &for_stat.step {
                        self.print(", ");
                        self.format_expression(step_expr);
                    }
                    self.print(" do");
                    self.indent();
                    self.newline();
                    self.format_block(&for_stat.body);
                    self.unindent();
                    self.print("end");
                }
                Stat::If(if_stat) => {
                    self.print("if ");
                    self.format_expression(&if_stat.condition);
                    self.print(" then");
                    self.indent();
                    self.newline();
                    self.format_block(&if_stat.then_block);
                    if let Some(else_block) = &if_stat.else_block {
                        self.unindent();
                        self.print("else");
                        self.indent();
                        self.newline();
                        self.format_block(else_block);
                    }
                    self.unindent();
                    self.print("end");
                }
                Stat::Call(call) => self.format_call(call),
                Stat::Return(return_stat) => {
                    self.print("return");
                    if !return_stat.values.is_empty() {
                        self.print(" ");
                        for (index, expr) in return_stat.values.iter().enumerate() {
                            self.format_expression(expr);
                            if index + 1 != return_stat.values.len() {
                                self.print(", ");
                            }
                        }
                    }
                }
                Stat::Break(_) => {
                    self.print("break");
                }
                Stat::Continue(_) => {
                    self.print("continue");
                }
                // TODO: we could optimize this by removing format and appending '-- ' directly to the output, if needed
                Stat::Comment(comment) => self.print(&format!("-- {}", &comment.comment)),
                _ => {}
            }
            self.newline();
        }
    }
}

pub fn format_ast(function: &Function) -> String {
    let mut formatter = Formatter::new();
    formatter.format_block(&function.body);
    formatter.output
}
