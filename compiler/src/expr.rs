use crate::lexer::{Token, TokenType};
use crate::typecheck::{Kind, Type};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        oper: Token,
        right: Box<Expr>,
    },
    Number {
        value: i32,
        meta: Token,
    },
    Variable(Token),
    Boolean {
        value: bool,
        meta: Token,
    },
    String {
        value: String,
        meta: Token,
    },
    Call {
        callee: Token,
        args: Vec<Expr>,
    },
    Assign {
        name: Box<Expr>,
        value: Box<Expr>,
    },
    Unary {
        oper: Token,
        right: Box<Expr>,
    },
    Group(Box<Expr>),
}

impl Expr {
    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Expr::Number { .. }
                | Expr::Boolean { .. }
                | Expr::String { .. }
        )
    }
}

impl From<&Expr> for Token {
    fn from(value: &Expr) -> Token {
        match value {
            Expr::Binary { oper, .. } => oper.clone(),
            Expr::Variable(name) => name.clone(),
            Expr::Call { callee, .. } => callee.clone(),
            Expr::Assign { value, .. } => Token::from(value.as_ref()),
            Expr::Group(expr) => Token::from(expr.as_ref()),
            Expr::Number { meta, .. } => meta.clone(),
            Expr::String { meta, .. } => meta.clone(),
            Expr::Boolean { meta, .. } => meta.clone(),
            Expr::Unary { oper, .. } => oper.clone(),
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum BinaryOperand {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Compare(Comparison),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Comparison {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EqualEqual,
    BangEqual,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum UnaryOperand {
    Negate,
    LogicalNot,
}

impl From<&Token> for UnaryOperand {
    fn from(token: &Token) -> UnaryOperand {
        match token.kind {
            TokenType::Bang => UnaryOperand::LogicalNot,
            TokenType::Minus => UnaryOperand::Negate,
            _ => panic!("Cannot turn {token} into UnaryOperand."),
        }
    }
}

impl std::fmt::Display for UnaryOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnaryOperand::Negate => write!(f, "-"),
            UnaryOperand::LogicalNot => write!(f, "!"),
        }
    }
}

impl std::fmt::Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Comparison::*;
        match self {
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            EqualEqual => write!(f, "=="),
            BangEqual => write!(f, "!="),
        }
    }
}

impl std::fmt::Display for BinaryOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BinaryOperand::*;
        match self {
            Add => write!(f, "+"),
            Subtract => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Modulus => write!(f, "%"),
            Compare(oper) => write!(f, "{oper}"),
        }
    }
}

impl From<&Token> for BinaryOperand {
    fn from(token: &Token) -> BinaryOperand {
        match token.kind {
            TokenType::Plus => BinaryOperand::Add,
            TokenType::Minus => BinaryOperand::Subtract,
            TokenType::Star => BinaryOperand::Multiply,
            TokenType::Slash => BinaryOperand::Divide,
            TokenType::Percent => BinaryOperand::Modulus,
            TokenType::Less => BinaryOperand::Compare(Comparison::Less),
            TokenType::LessEqual => {
                BinaryOperand::Compare(Comparison::LessEqual)
            }
            TokenType::Greater => {
                BinaryOperand::Compare(Comparison::Greater)
            }
            TokenType::GreaterEqual => {
                BinaryOperand::Compare(Comparison::GreaterEqual)
            }
            TokenType::EqualEqual => {
                BinaryOperand::Compare(Comparison::EqualEqual)
            }
            TokenType::BangEqual => {
                BinaryOperand::Compare(Comparison::BangEqual)
            }
            _ => panic!("Cannot turn {token} into binary operand."),
        }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum LLExpr {
    Binary {
        left: Box<LLExpr>,
        oper: BinaryOperand,
        right: Box<LLExpr>,
        ty_pe: Type,
    },
    Number(i32),
    Variable {
        name: String,
        ty_pe: Type,
        is_mutable: bool,
        kind: Kind,
    },
    Boolean(bool),
    String(String),
    Call {
        callee: String,
        args: Vec<LLExpr>,
        ty_pe: Type,
    },
    Assign {
        value: Box<LLExpr>,
        index: u16,
        ty_pe: Type,
    },
    Unary {
        oper: UnaryOperand,
        right: Box<LLExpr>,
        ty_pe: Type,
    },
    Group {
        value: Box<LLExpr>,
        ty_pe: Type,
    },
}

impl LLExpr {
    pub fn to_type(&self) -> Type {
        match self {
            LLExpr::Number(_) => Type::Number,
            LLExpr::String(_) => Type::String,
            LLExpr::Boolean(_) => Type::Bool,
            LLExpr::Variable { ty_pe, .. } => *ty_pe,
            LLExpr::Call { ty_pe, .. } => *ty_pe,
            LLExpr::Assign { ty_pe, .. } => *ty_pe,
            LLExpr::Unary { ty_pe, .. } => *ty_pe,
            LLExpr::Group { ty_pe, .. } => *ty_pe,
            LLExpr::Binary { ty_pe, .. } => *ty_pe,
        }
    }
}

// FIXME: Implementing is incorrect for non-primitive types
impl std::fmt::Debug for LLExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            LLExpr::Variable { name, .. } => write!(f, "{name}"),
            LLExpr::Binary {
                left, oper, right, ..
            } => {
                write!(f, "{:?}", *left)?;
                write!(f, "{:?}", oper)?;
                write!(f, "{:?}", *right)
            }
            LLExpr::Call { .. } => write!(f, "Function call expression."),
            LLExpr::Assign { value, .. } => write!(f, "{value:?}"),
            LLExpr::Group { value, .. } => write!(f, "{value:?}"),
            LLExpr::Unary { right, .. } => write!(f, "{right:?}"),
        }
    }
}
