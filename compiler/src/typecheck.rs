use crate::error::{BobaError, TypeError};
use crate::expr::{Expr, LLExpr};
use crate::lexer::{Token, Span};
use crate::stmt::{LLStmt, Parameter, Stmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Debug)]
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

    fn error(&mut self, msg: &str, span: Span) {
        self.errors.push(BobaError::TypeError {
            msg: msg.into(),
            span
        });
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        _then: &Stmt,
        _elze: &Option<Box<Stmt>>,
    ) {
        if Type::Bool != self.expression(condition) {
            self.error("Expected boolean expression as condition", Span::default());
        }
    }

    fn expression(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number(_) => Type::Number,
            Expr::Boolean(_) => Type::Bool,
            Expr::String(_) => Type::String,
            _ => todo!(),
        }
    }
}
