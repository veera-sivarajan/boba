use crate::analyzer::{Kind, Type};
use crate::lexer::Token;

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
    },
    Assign {
        name: Box<Expr>,
        value: Box<Expr>,
    },
}

impl From<&Expr> for Token {
    fn from(value: &Expr) -> Token {
        match value {
            Expr::Binary { oper, .. } => oper.clone(),
            Expr::Variable(name) => name.clone(),
            Expr::Call { callee, .. } => callee.clone(),
            Expr::Assign { name, .. } => Token::from(name.as_ref()),
            _ => panic!("Cannot turn value into token."),
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
            Expr::Assign { name, .. } => {
                write!(f, "{name}")
            }
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
        is_mutable: bool,
    },
    Boolean(bool),
    // String(String),
    Call {
        callee: String,
        args: Vec<LLExpr>,
    },
    Assign {
        value: Box<LLExpr>,
        index: u8,
    },
}

impl fmt::Display for LLExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LLExpr::Number(value) => write!(f, "{value}"),
            // LLExpr::String(value) => write!(f, "{value}"),
            LLExpr::Boolean(value) => {
                if *value {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            }
            LLExpr::Variable { name, .. } => write!(f, "{name}"),
            LLExpr::Binary { left, oper, right } => {
                write!(f, "{}", *left)?;
                write!(f, "{}", oper.kind)?;
                write!(f, "{}", *right)
            }
            LLExpr::Call { .. } => write!(f, "Function call expression."),
            LLExpr::Assign { value, .. } => write!(f, "{value}"),
        }
    }
}

impl From<&LLExpr> for String {
    fn from(value: &LLExpr) -> Self {
        value.to_string()
    }
}
