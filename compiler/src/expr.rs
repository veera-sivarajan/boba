use crate::lexer::Token;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        oper: Token,
        right: Box<Expr>,
    },
    Number {
        value: i64,
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
        matches!(self, Expr::Number { .. } | Expr::Boolean { .. } | Expr::String { .. })
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

// #[derive(Debug, Clone, Eq, Hash, PartialEq)]
// pub enum BinaryOperand {
//     Add,
//     Subtract,
//     Multiply,
//     Divide,
//     Modulus,
//     Compare(Comparison),
// }

// #[derive(Debug, Clone, Eq, Hash, PartialEq)]
// pub enum Comparison {
//     Less,
//     LessEqual,
//     Greater,
//     GreaterEqual,
//     EqualEqual,
// }

// #[derive(Debug, Clone, Eq, Hash, PartialEq)]
// pub enum UnaryOperand {
//     Negate,
//     LogicalNot,
// }

// impl From<&Token> for UnaryOperand {
//     fn from(token: &Token) -> UnaryOperand {
//         match token.kind {
//             TokenType::Bang => UnaryOperand::LogicalNot,
//             TokenType::Minus => UnaryOperand::Negate,
//             _ => panic!("Cannot turn {token} into UnaryOperand."),
//         }
//     }
// }

// impl std::fmt::Display for UnaryOperand {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         match self {
//             UnaryOperand::Negate => write!(f, "-"),
//             UnaryOperand::LogicalNot => write!(f, "!"),
//         }
//     }
// }

// impl std::fmt::Display for Comparison {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         use Comparison::*;
//         match self {
//             Less => write!(f, "<"),
//             LessEqual => write!(f, "<="),
//             Greater => write!(f, ">"),
//             GreaterEqual => write!(f, ">="),
//             EqualEqual => write!(f, "=="),
//         }
//     }
// }

// impl std::fmt::Display for BinaryOperand {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         use BinaryOperand::*;
//         match self {
//             Add => write!(f, "+"),
//             Subtract => write!(f, "-"),
//             Multiply => write!(f, "*"),
//             Divide => write!(f, "/"),
//             Modulus => write!(f, "%"),
//             Compare(oper) => write!(f, "{oper}"),
//         }
//     }
// }

// impl From<&Token> for BinaryOperand {
//     fn from(token: &Token) -> BinaryOperand {
//         match token.kind {
//             TokenType::Plus => BinaryOperand::Add,
//             TokenType::Minus => BinaryOperand::Subtract,
//             TokenType::Star => BinaryOperand::Multiply,
//             TokenType::Slash => BinaryOperand::Divide,
//             TokenType::Percent => BinaryOperand::Modulus,
//             TokenType::Less => BinaryOperand::Compare(Comparison::Less),
//             TokenType::LessEqual => {
//                 BinaryOperand::Compare(Comparison::LessEqual)
//             }
//             TokenType::Greater => BinaryOperand::Compare(Comparison::Greater),
//             TokenType::GreaterEqual => {
//                 BinaryOperand::Compare(Comparison::GreaterEqual)
//             }
//             TokenType::EqualEqual => {
//                 BinaryOperand::Compare(Comparison::EqualEqual)
//             }
//             _ => panic!("Cannot turn {token} into binary operand."),
//         }
//     }
// }

// #[derive(Clone, Eq, Hash, PartialEq)]
// pub enum LLExpr {
//     Binary {
//         left: Box<LLExpr>,
//         oper: BinaryOperand,
//         right: Box<LLExpr>,
//     },
//     Number(i64),
//     Variable {
//         name: String,
//         ty_pe: Type,
//         kind: Kind,
//         is_mutable: bool,
//     },
//     Boolean(bool),
//     String(String),
//     Call {
//         callee: String,
//         args: Vec<LLExpr>,
//     },
//     Assign {
//         value: Box<LLExpr>,
//         index: u8,
//     },
//     Unary {
//         oper: UnaryOperand,
//         right: Box<LLExpr>,
//     },
//     Group(Box<LLExpr>),
// }

// impl std::fmt::Debug for LLExpr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             LLExpr::Number(value) => write!(f, "{value}"),
//             LLExpr::String(value) => write!(f, "{value}"),
//             LLExpr::Boolean(value) => {
//                 if *value {
//                     write!(f, "1")
//                 } else {
//                     write!(f, "0")
//                 }
//             }
//             LLExpr::Variable { name, .. } => write!(f, "{name}"),
//             LLExpr::Binary { left, oper, right } => {
//                 write!(f, "{:?}", *left)?;
//                 write!(f, "{:?}", oper)?;
//                 write!(f, "{:?}", *right)
//             }
//             LLExpr::Call { .. } => write!(f, "Function call expression."),
//             LLExpr::Assign { value, .. } => write!(f, "{value:?}"),
//             LLExpr::Unary { oper, right } => write!(f, "{oper}{:?}", *right),
//             LLExpr::Group(expr) => {
//                 write!(f, "{:?}", *expr)
//             }
//         }
//     }
// }
