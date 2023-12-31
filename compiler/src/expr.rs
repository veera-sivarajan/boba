use crate::lexer::{Span, Token, TokenType};
use crate::typecheck::{Kind, Type};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Expr {
    Array {
        elements: Vec<Expr>,
        span: Span,
    },
    Binary {
        left: Box<Expr>,
        oper: Token,
        right: Box<Expr>,
    },
    Char {
        value: char,
        meta: Token,
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
    Subscript {
        name: Box<Expr>,
        index: Box<Expr>,
    },
}

impl Expr {
    pub fn is_constant(&self) -> bool {
        matches!(
            self,
            Expr::Number { .. } | Expr::Boolean { .. } | Expr::String { .. }
        )
    }
}

// this is disgusting. refactor this asap
impl From<&Expr> for Token {
    fn from(value: &Expr) -> Token {
        match value {
            Expr::Binary { oper, .. } => oper.clone(),
            Expr::Variable(name) => name.clone(),
            Expr::Call { callee, .. } => callee.clone(),
            Expr::Assign { value, .. } => Token::from(value.as_ref()),
            Expr::Group(expr) => Token::from(expr.as_ref()),
            Expr::Number { meta, .. } => meta.clone(),
            Expr::Char { meta, .. } => meta.clone(),
            Expr::String { meta, .. } => meta.clone(),
            Expr::Boolean { meta, .. } => meta.clone(),
            Expr::Unary { oper, .. } => oper.clone(),
            Expr::Array { span, .. } => Token::new(TokenType::Unknown, *span),
            Expr::Subscript { name, .. } => Token::from(name.as_ref()),
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
    Logic(Logical),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Logical {
    And,
    Or,
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

impl std::fmt::Display for Logical {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Logical::And => write!(f, "&&"),
            Logical::Or => write!(f, "||"),
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
            Logic(oper) => write!(f, "{oper}"),
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
            TokenType::Greater => BinaryOperand::Compare(Comparison::Greater),
            TokenType::GreaterEqual => {
                BinaryOperand::Compare(Comparison::GreaterEqual)
            }
            TokenType::EqualEqual => {
                BinaryOperand::Compare(Comparison::EqualEqual)
            }
            TokenType::BangEqual => {
                BinaryOperand::Compare(Comparison::BangEqual)
            }
            TokenType::And => BinaryOperand::Logic(Logical::And),
            TokenType::Or => BinaryOperand::Logic(Logical::Or),
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
    Char(char),
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
        index: usize,
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
    Array {
        ty_pe: Type,
        elements: Vec<LLExpr>,
        index: usize,
    },
    Subscript {
        base: Box<LLExpr>,
        ty_pe: Type,
        index: Box<LLExpr>,
    },
}

impl LLExpr {
    pub fn to_type(&self) -> Type {
        match self {
            LLExpr::Char(_) => Type::Char,
            LLExpr::Number(_) => Type::Number,
            LLExpr::String(_) => Type::String,
            LLExpr::Boolean(_) => Type::Bool,
            LLExpr::Variable { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Call { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Assign { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Unary { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Group { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Binary { ty_pe, .. } => ty_pe.clone(),
            LLExpr::Array {
                ty_pe, elements, ..
            } => Type::Array {
                ty_pe: Box::new(ty_pe.clone()),
                len: elements.len(),
            },
            LLExpr::Subscript { ty_pe, .. } => ty_pe.clone(),
        }
    }
}

// FIXME: Implementing is incorrect for non-primitive types
impl std::fmt::Debug for LLExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LLExpr::Char(value) => write!(f, "{value}"),
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
            LLExpr::Array { elements, .. } => {
                writeln!(f, "[")?;
                for ele in elements {
                    write!(f, " {ele:?} ")?;
                }
                write!(f, "]")
            }
            LLExpr::Subscript {
                base: array, index, ..
            } => {
                write!(f, "{array:?}[{index:?}]")
            }
        }
    }
}
