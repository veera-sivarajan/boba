use crate::expr::Expr;
use crate::lexer::Token;

pub type Parameter = (Expr, Token);

#[derive(Clone, Debug)]
pub enum Stmt {
    LocalVariable {
        name: Token,
        is_mutable: bool,
        init: Expr,
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
        params: Vec<Parameter>,
        return_type: Token,
        body: Box<Stmt>,
    }
}

impl From<&Stmt> for Token {
    fn from(stmt: &Stmt) -> Token {
        match stmt {
            Stmt::LocalVariable { name, .. } => name.clone(),
            Stmt::GlobalVariable { name, .. } => name.clone(),
            Stmt::Function { name, .. } => name.clone(),
            _ => panic!("Cannot turn stmt into token."),
        }
    }
}

