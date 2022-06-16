use cfg_ir::function::Function;

pub mod lifter;

pub fn lift(cfg: &Function) -> String {
    let ast_function = lifter::lift(cfg);
    ast_ir::formatter::format_ast(&ast_function)
}
