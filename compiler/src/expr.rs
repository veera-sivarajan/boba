use crate::lexer::Token;

#[derive(Clone, Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        oper: Token,
        right: Box<Expr>,
    }
}
