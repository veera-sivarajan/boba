use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::Token;
use crate::stmt::{Parameter, Stmt};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Kind {
    GlobalVariable,
    LocalVariable(u8),
    Parameter(u8),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Type {
    Number
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Info {
    kind: Kind,
    ty_pe: Type,
    is_mutable: bool,
}

impl Info {
    fn new(kind: Kind, is_mutable: bool) -> Self {
        Info {
            kind,
            ty_pe: Type::Number,
            is_mutable,
        }
    }
}

pub struct Analyzer {
    symbol_table: Vec<HashMap<String, Info>>,
    functions: Vec<Token>,
    variable_index: u8,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: vec![HashMap::new()],
            functions: vec![],
            variable_index: 0,
        }
    }

    pub fn check(&mut self, ast: &mut [Stmt]) -> Result<(), BobaError> {
        self.declare_all_globals(ast)?;
        self.main_is_defined()?;
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
                    if self.variable_is_declared_in_current_scope(name) {
                        return Err(BobaError::VariableRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        self.add_variable(
                            name,
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

    fn statement(&mut self, stmt: &mut Stmt) -> Result<(), BobaError> {
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

    fn expression(&mut self, mut expr: &mut Expr) -> Result<(), BobaError> {
        match &mut expr {
            Expr::Number(_) | Expr::Boolean(_) | Expr::String(_) => Ok(()),
            // Expr::Variable(token) => self.variable(token),
            Expr::Variable { name, ty_pe, kind } => {
                // self.variable(name, ty_pe, kind)},
                if let Some(info) = self.get_info(name) {
                    *ty_pe = Some(info.ty_pe.clone());
                    *kind =  Some(info.kind.clone());
                    Ok(())
                } else {
                    Err(BobaError::UndeclaredVariable(name.clone()))
                }
            }
            Expr::Call { callee, args } => self.function_call(callee, args),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
        }
    }

    fn binary(
        &mut self,
        left: &mut Expr,
        _oper: &Token,
        right: &mut Expr,
    ) -> Result<(), BobaError> {
        self.expression(left)?;
        self.expression(right)?;
        Ok(())
    }

    fn function_call(
        &mut self,
        callee: &Token,
        args: &mut [Expr],
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

    // fn variable(&mut self, name: &mut Token, ty_pe: &mut Option<Type>, mut kind: &mut Option<Kind>) -> Result<(), BobaError> {
    //     if let Some(info) = self.get_info(name) {
    //         ty_pe = &mut Some(info.ty_pe.clone()).clone();
    //         kind = &mut Some(info.kind.clone()).clone();
    //         Ok(())
    //     } else {
    //         Err(BobaError::UndeclaredVariable(name.clone()))
    //     }
    // }

    fn new_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.symbol_table.pop();
    }

    fn block_stmt(
        &mut self,
        stmts: &mut [Stmt],
        params: Option<&[Parameter]>,
    ) -> Result<(), BobaError> {
        self.new_scope();
        if let Some(params) = params {
            for (param, _param_type) in params {
                if self.variable_is_declared_in_current_scope(&param.into()) {
                    return Err(BobaError::VariableRedeclaration(param.into()));
                } else {
                    let variable_index = self.get_variable_index();
                    self.add_variable(
                        &param.into(),
                        false,
                        Kind::Parameter(variable_index),
                    );
                }
            }
        }
        self.check(stmts)?;
        self.exit_scope();
        Ok(())
    }

    fn if_stmt(
        &mut self,
        then: &mut Stmt,
        elze: &mut Option<Box<Stmt>>,
        condition: &mut Expr,
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
        body: &mut Stmt,
        params: &[Parameter],
    ) -> Result<(), BobaError> {
        let Stmt::Block(stmts) = body else {
            unreachable!("Expected function body to be a block statement.")
        };
        self.variable_index = 0;
        self.block_stmt(stmts, Some(params))?;
        Ok(())
    }

    fn global_variable_decl(&mut self, init: &mut Expr) -> Result<(), BobaError> {
        self.expression(init)?;
        Ok(())
    }

    fn get_variable_index(&mut self) -> u8 {
        let value = self.variable_index;
        self.variable_index += 1;
        value
    }

    fn local_variable_decl(
        &mut self,
        name: &Token,
        is_mutable: bool,
        init: &mut Expr,
        _index: u8,
    ) -> Result<(), BobaError> {
        if self.variable_is_declared_in_current_scope(name) {
            Err(BobaError::VariableRedeclaration(name.clone()))
        } else {
            self.expression(init)?;
            let variable_index = self.get_variable_index();
            self.add_variable(name, is_mutable, Kind::LocalVariable(variable_index));
            Ok(())
        }
    }

    fn is_global_scope(&self) -> bool {
        self.symbol_table.len() == 1
    }

    fn get_info(&self, name: &Token) -> Option<&Info> {
        for scope in self.symbol_table.iter().rev() {
            if scope.contains_key(&name.to_string()) {
                return scope.get(&name.to_string());
            } else {
                continue;
            }
        }
        None
    }

    fn variable_is_declared_in_current_scope(&self, name: &Token) -> bool {
        let current_scope = self.symbol_table.last().expect("No Symbol table for current scope.");
        current_scope.contains_key(&name.to_string())
    }

    fn variable_is_declared(&self, name: &Token) -> bool {
        for scope in self.symbol_table.iter().rev() {
            if scope.contains_key(&name.to_string()) {
                return true;
            } else {
                continue;
            }
        }
        false
    }

    fn add_variable(&mut self, name: &Token, is_mutable: bool, kind: Kind) {
        let scope = self.symbol_table.last_mut().expect("Symbol table is empty.");
        scope.insert(name.to_string(), Info::new(kind, is_mutable));
    }
}
