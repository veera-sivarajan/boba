use crate::error::{BobaError, TypeError};
use crate::expr::{Expr};
use crate::lexer::{Token, Span};
use crate::stmt::{Parameter, Stmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum Type {
    Number,
    String,
    Bool,
}

pub struct TypeChecker {
    env: Vec<HashMap<Token, Type>>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: vec![HashMap::new()],
            errors: vec![],
        }
    }

    fn exit_typechecker(&self) -> Result<(), BobaError> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(BobaError::TypeCheck(self.errors.clone()))
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            self.statement(stmt);
        }
        self.exit_typechecker()
    }

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            Stmt::Function { body, .. } => self.statement(body),
            Stmt::Block(stmts) => self.block(stmts),
            _ => todo!(),
        }
    }

    fn block(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.statement(stmt);
        }
    }

    fn error(&mut self, expected: Type, found: Type, span: Span) {
        self.errors.push(TypeError::Mismatched {
            expected,
            found,
            span,
        });
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        _then: &Stmt,
        _elze: &Option<Box<Stmt>>,
    ) {

        let cond_type = self.expression(condition);
        if Type::Bool != cond_type {
            let token = Token::from(condition);
            self.error(Type::Bool, cond_type, token.span);
        }
    }

    fn expression(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number { .. } => Type::Number,
            Expr::Boolean { .. } => Type::Bool,
            Expr::String { .. } => Type::String,
            _ => todo!(),
        }
    }
}
