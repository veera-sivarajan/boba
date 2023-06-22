use crate::expr::Expr;
use crate::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: Token,
        is_mutable: bool,
        init: Option<Expr>,
    },
    Expression(Expr),
    Print(Expr),
    If {
        condition: Expr,
        then: Box<Stmt>,
        elze: Option<Box<Stmt>>,
    },
    Block(Vec<Stmt>),
}
