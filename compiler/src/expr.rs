use crate::lexer::Token;
use crate::analyzer::{Kind, Type};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        oper: Token,
        right: Box<Expr>,
    },
    Number(i64),
    Variable(Token),
    Boolean(bool),
    String(String),
    Call {
        callee: Token,
        args: Vec<Expr>,
    }
}

// impl From<&mut Expr> for Token {
//     fn from(value: &mut Expr) -> Token {
//         match value {
//             Expr::Binary { oper, .. } => oper.clone(),
//             Expr::Variable(name) => name.clone(),
//             Expr::Call { callee, .. } => callee.clone(),
//             _ => panic!("Cannot turn value into token.")
//         }
//     }
// }


impl From<&Expr> for Token {
    fn from(value: &Expr) -> Token {
        match value {
            Expr::Binary { oper, .. } => oper.clone(),
            Expr::Variable(name) => name.clone(),
            Expr::Call { callee, .. } => callee.clone(),
            _ => panic!("Cannot turn value into token.")
        }
    }
}

use std::fmt;
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(value) => write!(f, "{value}"),
            Expr::String(value) => write!(f, "{value}"),
            Expr::Boolean(value) => {
                if *value {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            }
            Expr::Variable(name) => write!(f, "{name}"),
            Expr::Binary { left, oper, right } => {
                write!(f, "{}", *left)?;
                write!(f, "{}", oper.kind)?;
                write!(f, "{}", *right)
            }
            Expr::Call { .. } => write!(f, "Function call expression."),
        }
    }
}

impl From<&Expr> for String {
    fn from(value: &Expr) -> Self {
        value.to_string()
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum LLExpr {
    Binary {
        left: Box<LLExpr>,
        oper: Token,
        right: Box<LLExpr>,
    },
    Number(i64),
    Variable {
        name: String,
        ty_pe: Type,
        kind: Kind,
    },
    Boolean(bool),
    String(String),
    Call {
        callee: String,
        args: Vec<LLExpr>,
    }
}

impl fmt::Display for LLExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LLExpr::Number(value) => write!(f, "{value}"),
            LLExpr::String(value) => write!(f, "{value}"),
            LLExpr::Boolean(value) => {
                if *value {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            }
            LLExpr::Variable{ name, .. } => write!(f, "{name}"),
            LLExpr::Binary { left, oper, right } => {
                write!(f, "{}", *left)?;
                write!(f, "{}", oper.kind)?;
                write!(f, "{}", *right)
            }
            LLExpr::Call { .. } => write!(f, "Function call expression."),
        }
    }
}

impl From<&LLExpr> for String {
    fn from(value: &LLExpr) -> Self {
        value.to_string()
    }
}
