use crate::error::{BobaError, TypeError};
use crate::expr::Expr;
use crate::lexer::{Span, Token, TokenType};
use crate::stmt::{Parameter, Stmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum Type {
    Number,
    String,
    Bool,
    Unknown,
}

pub struct TypeChecker {
    type_table: Vec<HashMap<Token, Info>>,
    errors: Vec<BobaError>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Info {
    ty_pe: Type,
    is_mutable: bool,
}

impl Info {
    fn new(ty_pe: Type, is_mutable: bool) -> Self {
        Info { ty_pe, is_mutable }
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            type_table: vec![HashMap::new()],
            errors: vec![],
        }
    }

    fn exit(&self) -> Result<(), BobaError> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(BobaError::Analyzer(self.errors.clone()))
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            self.statement(stmt);
        }
        self.exit()
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
            Stmt::LocalVariable {
                name,
                init,
                is_mutable,
            } => self.local_variable(name, init, *is_mutable),
            _ => todo!(),
        }
    }

    fn add_variable(&mut self, name: Token, info: Info) {
        let scope = self.type_table.last_mut().expect("Type table is empty.");
        scope.insert(name, info);
    }

    fn local_variable(&mut self, name: &Token, init: &Expr, is_mutable: bool) {
        let init = self.expression(init);
        self.add_variable(name.clone(), Info::new(init, is_mutable));
    }

    fn block(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.statement(stmt);
        }
    }

    fn error_if_ne(&mut self, expected: Type, found: Type, span: Span) {
        if found != Type::Unknown && expected != found {
            self.errors.push(BobaError::TypeCheck(TypeError::Mismatched {
                expected,
                found,
                span,
            }));
        }
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        then: &Stmt,
        elze: &Option<Box<Stmt>>,
    ) {
        let cond_type = self.expression(condition);
        self.error_if_ne(Type::Bool, cond_type, Token::from(condition).span);
        self.statement(then);
        if let Some(stmt) = elze {
            self.statement(stmt);
        };
    }

    fn expression(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Number { .. } => Type::Number,
            Expr::Boolean { .. } => Type::Bool,
            Expr::String { .. } => Type::String,
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Assign { name, value } => self.assign(name, value),
            Expr::Variable(token) => self.variable(token),
            _ => todo!(),
        }
    }

    fn assign(&mut self, name: &Expr, value: &Expr) -> Type {
        let name = self.expression(name);
        let value_ty = self.expression(value);
        self.error_if_ne(name, value_ty.clone(), Token::from(value).span);
        value_ty
    }

    fn get_info(&self, name: &Token) -> Option<&Info> {
        for scope in self.type_table.iter().rev() {
            if scope.contains_key(name) {
                return scope.get(name);
            } else {
                continue;
            }
        }
        None
    }

    fn error(&mut self, error: BobaError) {
        self.errors.push(error);
    }

    fn variable(&mut self, token: &Token) -> Type {
        if let Some(ty) = self.get_info(token).map(|info| info.ty_pe.clone()) {
            ty
        } else {
            self.error(BobaError::UndeclaredVariable(token.clone()));
            Type::Unknown
        }
    }

    fn binary(&mut self, left: &Expr, oper: &Token, right: &Expr) -> Type {
        let left = self.expression(left);
        let right = self.expression(right);
        self.error_if_ne(Type::Number, left, oper.span);
        self.error_if_ne(Type::Number, right, oper.span);
        match oper.kind {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::Percent => Type::Number,
            TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::EqualEqual => Type::Bool,
            _ => unreachable!(),
        }
    }
}
