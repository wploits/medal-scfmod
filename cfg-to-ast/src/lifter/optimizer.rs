fn block_immediately_breaks(body: &ast_ir::Block) -> bool {
    body.statements.len() == 1 && matches!(body.statements[0], ast_ir::Stat::Break(_))
}

fn combine_conditions<'ast>(first: ast_ir::Expr<'ast>, second: ast_ir::Expr<'ast>) -> ast_ir::Expr<'ast> {
    if let ast_ir::Expr::Lit(lit) = &first {
        if lit.lit == ast_ir::Lit::Boolean(true) {
            return second;
        }
    }
    ast_ir::Binary {
        pos: None,
        op: ast_ir::BinaryOp::LogicalAnd,
        lhs: Box::new(first),
        rhs: Box::new(second),
    }
    .into()
}

pub(super) fn optimize_while(while_stat: &mut ast_ir::While) {
    let mut changed = true;
    while changed {
        changed = false;
        let stats = &mut while_stat.body.statements;
        if stats.len() == 1 {
            if let ast_ir::Stat::If(if_stat) = stats.get_mut(0).unwrap() {
                if let Some(else_block) = &if_stat.else_block {
                    if block_immediately_breaks(else_block) {
                        while_stat.condition = combine_conditions(
                            // TODO: find a way to do this without cloning
                            while_stat.condition.clone(),
                            if_stat.condition.clone(),
                        );
                        while_stat.body = if_stat.then_block.clone();
                        changed = true;
                    } else if block_immediately_breaks(&if_stat.then_block) {
                        while_stat.condition = combine_conditions(
                            // TODO: find a way to do this without cloning
                            while_stat.condition.clone(),
                            ast_ir::Unary {
                                pos: None,
                                op: ast_ir::UnaryOp::LogicalNot,
                                expr: Box::new(if_stat.condition.clone()),
                            }
                            .into(),
                        );
                        while_stat.body = else_block.clone();
                        changed = true;
                    }
                }
            }
        }
    }
}
