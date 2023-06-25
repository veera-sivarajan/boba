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

#[derive(Debug)]
enum Kind {
    GlobalVariable,
    LocalVariable,
    Function,
}

pub struct Analyzer {
    scopes: Vec<HashMap<Symbol, Kind>>,
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
            Stmt::Block(stmts) => {
                self.scopes.push(HashMap::new());
                self.check(stmts)?;
                self.scopes.pop();
                Ok(())
            }
            Stmt::Function { name, body, .. } => {
                self.function_decl(name, body)
            }
            Stmt::If { then, elze, ..} => {
                self.if_stmt(then, elze)
            }
            _ => todo!(),
        }
    }

    fn if_stmt(&mut self, then: &Stmt, elze: &Option<Box<Stmt>>) -> Result<(), BobaError> {
        self.check_stmt(then)?;
        if let Some(else_stmt) = elze {
            self.check_stmt(else_stmt)?;
        }
        Ok(())
    }

    fn function_decl(&mut self, name: &Token, body: &Stmt) -> Result<(), BobaError> {
        if self.variable_is_declared_in_current_scope(&name.to_string()) {
            Err(BobaError::VariableRedeclaration(name.clone()))
        } else {
            self.check_stmt(body)?;
            self.add_variable(&name.to_string(), false, Kind::Function);
            Ok(())
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
            let kind = if self.current_scope() == 0 {
                Kind::GlobalVariable
            } else {
                Kind::LocalVariable
            };
            self.add_variable(&name.to_string(), is_mutable, kind);
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
                    return Some(self.current_scope() - index as u8);
                }
            }
        }
        None
    }

    fn add_variable(&mut self, name: &str, is_mutable: bool, kind: Kind) {
        if let Some(map) = self.scopes.last_mut() {
            let name = name.to_string();
            map.insert(Symbol { name, is_mutable }, kind);
        }
    }
}
