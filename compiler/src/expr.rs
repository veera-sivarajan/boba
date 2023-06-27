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
    // Variable(Token),
    Variable {
        name: Token,
        index: Option<u8>,
        ty_pe: Option<Type>,
        kind: Option<Kind>,
    },
    Boolean(bool),
    String(String),
    Call {
        callee: Token,
        args: Vec<Expr>,
    }
}

impl Expr {
    fn new_variable(name: Token) -> Expr {
        Expr::Variable {
            name,
            index: None,
            ty_pe: None,
            kind: None,
        }
    }
}

impl From<&Expr> for Token {
    fn from(value: &Expr) -> Token {
        match value {
            Expr::Binary { oper, .. } => oper.clone(),
            Expr::Variable(token) => token.clone(),
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
            Expr::Variable(token) => write!(f, "{token}"),
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
            
