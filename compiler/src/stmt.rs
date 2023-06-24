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
        then: Vec<Stmt>,
        elze: Option<Vec<Stmt>>,
    },
    Block(Vec<Stmt>),
    Function {
        name: Token,
        params: Vec<(Token, Token)>,
        return_type: Token,
        body: Vec<Stmt>,
    }
}

