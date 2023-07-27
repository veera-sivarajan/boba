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

// impl From<&Stmt> for Token {
//     fn from(stmt: &Stmt) -> Token {
//         match stmt {
//             Stmt::LocalVariable { name, .. } => name.clone(),
//             Stmt::GlobalVariable { name, .. } => name.clone(),
//             Stmt::Function { name, .. } => name.clone(),
//             _ => panic!("Cannot turn stmt into token."),
//         }
//     }
// }

#[derive(Clone)]
pub enum LLStmt {
    LocalVariable {
        init: LLExpr,
        ty_pe: Type,
        variable_index: u16,
    },
    GlobalVariable {
        name: String,
        init: LLExpr,
    },
    Expression(LLExpr),
    Print {
        value: LLExpr,
        ty_pe: Type,
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
        space_for_locals: u16,
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
