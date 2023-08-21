use crate::expr::{Expr, LLExpr};
use crate::lexer::Token;
use crate::typecheck::Type;

pub type Parameter = (Expr, Type);

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
    Print {
        meta: Token,
        args: Vec<Expr>,
    },
    If {
        condition: Expr,
        then: Box<Stmt>,
        elze: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
    Function {
        name: Token,
        params: Vec<Parameter>,
        return_type: Type,
        body: Vec<Stmt>,
    },
    Return {
        name: Token,
        expr: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum LLStmt {
    LocalVariable {
        init: LLExpr,
        ty_pe: Type,
        variable_index: usize,
    },
    GlobalVariable {
        name: String,
        init: LLExpr,
    },
    Expression(LLExpr),
    Print {
        format: String,
        args: Vec<LLExpr>,
        arg_count: usize,
    },
    Block(Vec<LLStmt>),
    If {
        condition: LLExpr,
        then: Box<LLStmt>,
        elze: Option<Box<LLStmt>>,
    },
    Function {
        name: String,
        param_types: Vec<Type>,
        space_for_locals: usize,
        body: Box<LLStmt>,
        return_type: Type,
    },
    Return {
        name: String,
        value: LLExpr,
    },
    While {
        condition: LLExpr,
        body: Box<LLStmt>,
    },
    Error,
}
