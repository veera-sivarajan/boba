use crate::expr::Expr;
use crate::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    LocalVariable {
        name: Token,
        is_mutable: bool,
        init: Expr,
        index: u8,
    },
    GlobalVariable {
        name: Token,
        init: Expr,
    },
    Expression(Expr),
    Print(Expr),
    If {
        condition: Expr,
        then: Box<Stmt>,
        elze: Option<Box<Stmt>>,
    },
    Block(Vec<Stmt>),
    Function {
        name: Token,
        params: Vec<(Token, Token)>,
        return_type: Token,
        body: Box<Stmt>,
        num_locals: u8,
    }
}

