use crate::expr::{Expr, LLExpr};
use crate::lexer::Token;
use crate::analyzer::{Kind, Type};

pub type Parameter = (Expr, Token);

#[derive(Clone, Debug)]
pub enum Stmt {
    LocalVariable {
        name: Token,
        is_mutable: bool,
        init: Expr,
        ty_pe: Option<Type>,
        kind: Option<Kind>,
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
    }
}

impl Stmt {
    pub fn new_local(name: Token, is_mutable: bool, init: Expr) -> Self {
        Stmt::LocalVariable {
            name,
            is_mutable,
            init,
            ty_pe: None,
            kind: None,
        }
    }
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
        kind: Kind,
    },
    GlobalVariable {
        name: String,
        init: LLExpr,
    },
    Expression(LLExpr),
    Print(LLExpr),
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
}
        
       

