use crate::analyzer::Type;
use crate::expr::{Expr, LLExpr};
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
    },
    Return {
        name: Token,
        expr: Expr,
    },
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

#[derive(Clone, Debug)]
pub enum LLStmt {
    LocalVariable {
        init: LLExpr,
        ty_pe: Type,
        variable_index: u8,
    },
    GlobalVariable {
        name: String,
        init: LLExpr,
    },
    Expression(LLExpr),
    Print(LLExpr),
    Block(Vec<LLStmt>),
    If {
        condition: LLExpr,
        then: Box<LLStmt>,
        elze: Option<Box<LLStmt>>,
    },
    Function {
        name: String,
        params_count: u8,
        locals_count: u8,
        body: Box<LLStmt>,
    },
    Return {
        name: String,
        expr: LLExpr,
    },
}
