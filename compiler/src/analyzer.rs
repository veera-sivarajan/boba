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
}

pub struct Analyzer {
    scopes: Vec<HashMap<Symbol, Kind>>,
    functions: Vec<Token>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            functions: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        self.declare_all_functions(ast)?;
        for stmt in ast {
            self.statement(stmt)?;
        }
        Ok(())
    }

    fn add_function(&mut self, name: &Token) {
        self.functions.push(name.clone());
    }

    fn function_is_defined(&self, name: &Token) -> bool {
        self.functions.contains(name)
    }

    fn declare_all_functions(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            match stmt {
                Stmt::Function { name, .. } => {
                    if self.function_is_defined(name) {
                        return Err(BobaError::FunctionRedeclaration(name.clone()));
                    } else {
                        self.add_function(name);
                    }
                }
                _ => continue,
            }
        }
        Ok(())
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::Let {
                name,
                is_mutable,
                init,
            } => self.let_decl(name, *is_mutable, init),
            Stmt::Block(stmts) => self.block_stmt(stmts, None),
            Stmt::Function {
                name, body, params, ..
            } => self.function_decl(name, body, params),
            Stmt::If {
                then,
                elze,
                condition,
            } => self.if_stmt(then, elze, condition),
            Stmt::Expression(expr) | Stmt::Print(expr) => self.expression(expr),
        }
    }

    fn expression(&mut self, expr: &Expr) -> Result<(), BobaError> {
        match expr {
            Expr::Number(_) | Expr::Boolean(_) | Expr::String(_) => Ok(()),
            Expr::Variable(token) => self.variable(token),
            Expr::Call { callee, args } => self.function_call(callee, args),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
        }
    }

    fn binary(
        &mut self,
        left: &Expr,
        _oper: &Token,
        right: &Expr,
    ) -> Result<(), BobaError> {
        self.expression(left)?;
        self.expression(right)?;
        Ok(())
    }

    fn function_call(
        &mut self,
        callee: &Token,
        args: &[Expr],
    ) -> Result<(), BobaError> {
        if !self.function_is_defined(callee) {
            Err(BobaError::UndeclaredFunction(callee.clone()))
        } else {
            for arg in args {
                self.expression(arg)?;
            }
            Ok(())
        }
    }

    fn variable(&mut self, name: &Token) -> Result<(), BobaError> {
        if self.variable_is_declared(&name.to_string()).is_none() {
            Err(BobaError::UndeclaredVariable(name.clone()))
        } else {
            Ok(())
        }
    }

    fn block_stmt(
        &mut self,
        stmts: &[Stmt],
        params: Option<&[(Token, Token)]>,
    ) -> Result<(), BobaError> {
        self.scopes.push(HashMap::new());
        if let Some(params) = params {
            for (param, _param_type) in params {
                if self
                    .variable_is_declared_in_current_scope(&param.to_string())
                {
                    return Err(BobaError::VariableRedeclaration(
                        param.clone(),
                    ));
                } else {
                    self.add_variable(
                        &param.to_string(),
                        false,
                        Kind::LocalVariable,
                    );
                }
            }
        }
        self.check(stmts)?;
        self.scopes.pop();
        Ok(())
    }

    fn if_stmt(
        &mut self,
        then: &Stmt,
        elze: &Option<Box<Stmt>>,
        condition: &Expr,
    ) -> Result<(), BobaError> {
        self.expression(condition)?;
        self.statement(then)?;
        if let Some(else_stmt) = elze {
            self.statement(else_stmt)?;
        }
        Ok(())
    }

    fn function_decl(
        &mut self,
        name: &Token,
        body: &Stmt,
        params: &[(Token, Token)],
    ) -> Result<(), BobaError> {
        let Stmt::Block(stmts) = body else {
            unreachable!()
        };
        self.block_stmt(stmts, Some(params))?;
        Ok(())
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
            if let Some(expr) = init {
                self.expression(expr)?;
            }
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
            for (key, value) in scope {
                if key.name == name
                    && matches!(
                        value,
                        Kind::LocalVariable | Kind::GlobalVariable
                    )
                {
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
