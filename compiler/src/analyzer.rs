use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::Token;
use crate::stmt::{Parameter, Stmt};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum Kind {
    GlobalVariable,
    LocalVariable,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum Type {
    Number
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Symbol {
    name: String,
    kind: Kind,
    index: u8,
    ty_pe: Type,
    is_mutable: bool,
}

#[derive(Clone, Debug)]
pub struct Info {
    name: String,
    size: u8,
}

impl Info {
    fn new(token: Token) -> Info {
        Info {
            name: token.to_string(),
            size: 8,
        }
    }
}

pub struct Analyzer {
    symbol_table: Vec<HashMap<String, Symbol>>,
    functions: Vec<Token>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            functions: vec![],
            symbol_table: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<Vec<Vec<Info>>, BobaError> {
        self.declare_all_globals(ast)?;
        self.main_is_defined()?;
        for stmt in ast {
            self.statement(stmt)?;
        }
        Ok(self.symbol_table.clone())
    }

    fn add_function(&mut self, name: &Token) {
        self.functions.push(name.clone());
    }

    fn function_is_defined(&self, name: &Token) -> bool {
        self.functions.contains(name)
    }

    fn declare_all_globals(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            match stmt {
                Stmt::Function { name, .. } => {
                    if self.function_is_defined(name) {
                        return Err(BobaError::FunctionRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        self.add_function(name);
                    }
                }
                Stmt::GlobalVariable { name, .. } => {
                    if self.variable_is_declared_in_current_scope(
                        &name.to_string(),
                    ) {
                        return Err(BobaError::VariableRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        self.add_variable(
                            &name.to_string(),
                            false,
                            Kind::GlobalVariable,
                        );
                    }
                }
                _ => continue,
            }
        }

        Ok(())
    }

    fn main_is_defined(&self) -> Result<(), BobaError> {
        if !self
            .functions
            .iter()
            .any(|decl| &decl.to_string() == "main")
        {
            Err(BobaError::MainNotFound)
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::LocalVariable {
                name,
                is_mutable,
                init,
                index,
            } => self.local_variable_decl(name, *is_mutable, init, *index),
            Stmt::GlobalVariable { init, .. } => {
                self.global_variable_decl(init)
            }
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

    fn add_to_symbol_table(&mut self, name: Token) {
        let map = self.symbol_table.last_mut().expect("Expected a symbol table.");
        map.push(Info::new(name));
    }

    fn block_stmt(
        &mut self,
        stmts: &[Stmt],
        params: Option<&[Parameter]>,
    ) -> Result<(), BobaError> {
        self.scopes.push(HashMap::new());
        if let Some(params) = params {
            for (param, _param_type) in params {
                if self
                    .variable_is_declared_in_current_scope(&param.to_string())
                {
                    return Err(BobaError::VariableRedeclaration(param.into()));
                } else {
                    self.add_to_symbol_table(param.into());
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
        _name: &Token,
        body: &Stmt,
        params: &[Parameter],
    ) -> Result<(), BobaError> {
        let Stmt::Block(stmts) = body else {
            unreachable!("Expected function body to be a block statement.")
        };
        self.symbol_table.push(vec![]);
        self.block_stmt(stmts, Some(params))?;
        Ok(())
    }

    fn global_variable_decl(&mut self, init: &Expr) -> Result<(), BobaError> {
        self.expression(init)?;
        Ok(())
    }

    fn local_variable_decl(
        &mut self,
        name: &Token,
        is_mutable: bool,
        init: &Expr,
        _index: u8,
    ) -> Result<(), BobaError> {
        if self.variable_is_declared_in_current_scope(&name.to_string()) {
            Err(BobaError::VariableRedeclaration(name.clone()))
        } else {
            let kind = if self.current_scope() == 0 {
                Kind::GlobalVariable
            } else {
                Kind::LocalVariable
            };
            self.expression(init)?;
            self.add_variable(&name.to_string(), is_mutable, kind);
            self.add_to_symbol_table(name.clone());
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
