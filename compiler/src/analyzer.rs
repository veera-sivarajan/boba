use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Symbol {
    name: String,
    is_mutable: bool,
}

enum Type {
    Unknown,
}

pub struct Analyzer {
    scopes: Vec<HashMap<Symbol, Type>>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::Let {
                name,
                is_mutable,
                init,
            } => self.let_decl(name, *is_mutable, init),
            _ => todo!(),
        }
    }

    fn let_decl(
        &mut self,
        name: &Token,
        is_mutable: bool,
        init: &Option<Expr>,
    ) -> Result<(), BobaError> {
        if self.variable_is_declared_in_current_scope(&name.to_string()) {
            Err(BobaError::VariableRedeclaration(name.clone()))
        } else {
            self.add_variable(&name.to_string(), is_mutable);
            Ok(())
        }
    }

    fn current_scope(&self) -> u8 {
        self.scopes.len() as u8 - 1
    }

    fn is_global_scope(&self) -> bool {
        self.current_scope() == 0
    }

    fn variable_is_declared_in_current_scope(&self, name: &str) -> bool {
        self.variable_is_declared(name)
            .is_some_and(|scope| scope == self.current_scope())
    }

    fn variable_is_declared(&self, name: &str) -> Option<u8> {
        for (index, scope) in self.scopes.iter().rev().enumerate() {
            for (key, _value) in scope {
                if key.name == name {
                    return Some(index as u8);
                }
            }
        }
        None
    }

    fn add_variable(&mut self, name: &str, is_mutable: bool) {
        if let Some(map) = self.scopes.last_mut() {
            let name = name.to_string();
            map.insert(Symbol { name, is_mutable }, Type::Unknown);
        }
    }
}
