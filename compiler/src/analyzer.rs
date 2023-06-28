use crate::error::BobaError;
use crate::expr::{Expr, LLExpr};
use crate::lexer::Token;
use crate::stmt::{Parameter, Stmt, LLStmt};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Kind {
    GlobalVariable,
    LocalVariable(u8),
    Parameter(u8),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Type {
    Number,
}

impl From<&Type> for u8 {
    fn from(value: &Type) -> u8 {
        match value {
            Type::Number => 8,
        }
    }
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
    statements: Vec<LLStmt>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: vec![HashMap::new()],
            functions: vec![],
            variable_index: 0,
            statements: vec![],
        }
    }

    pub fn check(&mut self, ast: &mut [Stmt]) -> Result<Vec<LLStmt>, BobaError> {
        self.declare_all_globals(ast)?;
        self.main_is_defined()?;
        for stmt in ast {
            let node = self.statement(stmt)?;
            self.statements.push(node);
        }
        Ok(self.statements.clone())
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
                        self.add_variable(name, false, Kind::GlobalVariable);
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

    fn statement(&mut self, stmt: &mut Stmt) -> Result<LLStmt, BobaError> {
        match stmt {
            Stmt::LocalVariable {
                name,
                is_mutable,
                init,
                ty_pe,
                kind,
            } => self.local_variable_decl(name, *is_mutable, init, ty_pe, kind),
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

    fn expression(&mut self, expr: &Expr) -> Result<LLExpr, BobaError> {
        match expr {
            Expr::Number(value) => Ok(LLExpr::Number(*value)),
            Expr::Boolean(value) => Ok(LLExpr::Boolean(*value)),
            Expr::String(value) => Ok(LLExpr::String(value.clone())),
            Expr::Variable { name, ty_pe, kind } => {
                self.variable(name, ty_pe, kind)
            }
            Expr::Call { callee, args } => self.function_call(callee, args),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
        }
    }

    fn binary(
        &mut self,
        left: &Expr,
        oper: &Token,
        right: &Expr,
    ) -> Result<LLExpr, BobaError> {
        let left = Box::new(self.expression(left)?);
        let right = Box::new(self.expression(right)?);
        Ok(LLExpr::Binary {
            left,
            oper: oper.clone(),
            right,
        })
    }

    fn function_call(
        &mut self,
        callee: &Token,
        args: &[Expr],
    ) -> Result<LLExpr, BobaError> {
        if !self.function_is_defined(callee) {
            Err(BobaError::UndeclaredFunction(callee.clone()))
        } else {
            let mut ll_args= vec![];
            for arg in args {
                ll_args.push(self.expression(arg)?);
            }
            Ok(LLExpr::Call {
                callee: callee.to_string(),
                args: ll_args,
            })
        }
    }

    fn variable(
        &mut self,
        name: &Token,
        ty_pe: &Option<Type>,
        kind: &Option<Kind>,
    ) -> Result<LLExpr, BobaError> {
        if let Some(info) = self.get_info(name) {
            Ok(LLExpr::Variable {
                name: name.to_string(),
                ty_pe: info.ty_pe.clone(),
                kind: info.kind.clone(),
            })
        } else {
            Err(BobaError::UndeclaredVariable(name.clone()))
        }
    }

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

    fn global_variable_decl(
        &mut self,
        init: &mut Expr,
    ) -> Result<(), BobaError> {
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
        ty_pe: &mut Option<Type>,
        kind: &mut Option<Kind>,
    ) -> Result<LLStmt, BobaError> {
        if self.variable_is_declared_in_current_scope(name) {
            Err(BobaError::VariableRedeclaration(name.clone()))
        } else {
            let init = self.expression(init)?;
            let variable_index = self.get_variable_index();
            self.add_variable(
                name,
                is_mutable,
                Kind::LocalVariable(variable_index),
            );
            // *ty_pe = Some(Type::Number);
            // *kind = Some(Kind::LocalVariable(variable_index));
            Ok(LLStmt::LocalVariable {
                init,
                ty_pe: Type::Number,
                kind: Kind::LocalVariable(variable_index),
            })
        }
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
        let current_scope = self
            .symbol_table
            .last()
            .expect("No Symbol table for current scope.");
        current_scope.contains_key(&name.to_string())
    }

    fn add_variable(&mut self, name: &Token, is_mutable: bool, kind: Kind) {
        let scope = self
            .symbol_table
            .last_mut()
            .expect("Symbol table is empty.");
        scope.insert(name.to_string(), Info::new(kind, is_mutable));
    }
}
