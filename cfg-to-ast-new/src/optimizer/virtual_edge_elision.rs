use ast::{If, Statement};

fn can_optimize(if_statement: &If) -> bool {
    if if_statement.then_block.is_none() && if_statement.else_block.is_none() {
        return false;
    }
    let (last_then, last_else) = (
        if_statement.then_block.as_ref().unwrap().last().unwrap(),
        if_statement.else_block.as_ref().unwrap().last().unwrap(),
    );
    match last_then {
        Statement::Continue(_) => matches!(last_else, Statement::Continue(_)),
        Statement::Break(_) => matches!(last_else, Statement::Break(_)),
        _ => false,
    }
}

pub(crate) fn optimize_if_statement<'a>(if_statement: &mut If<'a>) -> Option<Statement<'a>> {
    if can_optimize(if_statement) {
        if_statement.then_block.as_mut().unwrap().pop();
        Some(if_statement.else_block.as_mut().unwrap().pop().unwrap())
    } else {
        None
    }
}
