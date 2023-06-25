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
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    }
}

impl Expr {
    pub fn is_constant(&self) -> bool {
        match self {
            Expr::Number(_) | Expr::String(_) | Expr::Boolean(_) => true,
            _ => false,
        }
    }

    pub fn get_size(&self) -> Option<Box<str>> {
        match self {
            Expr::Number(_) | Expr::Boolean(_) => Some("quad".into()),
            Expr::String(_) => Some("string".into()),
            _ => None
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
            
