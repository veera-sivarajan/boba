use crate::lexer::Token;
use crate::expr::Expr;

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: Token,
        is_mutable: bool,
        init: Option<Expr>,
    },
    Expression(Expr),
}
