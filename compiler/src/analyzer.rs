use crate::error::BobaError;
use crate::expr::{Expr, LLExpr};
use crate::lexer::Token;
use crate::stmt::{LLStmt, Parameter, Stmt};
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

struct FunctionData {
    name: Token,
    params_count: usize,
}

impl FunctionData {
    fn new(name: Token, params_count: usize) -> Self {
        FunctionData { name, params_count }
    }
}

pub struct Analyzer {
    symbol_table: Vec<HashMap<String, Info>>,
    functions: Vec<FunctionData>,
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

    pub fn check(&mut self, ast: &[Stmt]) -> Result<Vec<LLStmt>, BobaError> {
        self.declare_all_globals(ast)?;
        self.main_is_defined()?;
        self.resolve(ast)
    }

    fn resolve(&mut self, ast: &[Stmt]) -> Result<Vec<LLStmt>, BobaError> {
        let mut result = vec![];
        for stmt in ast {
            result.push(self.statement(stmt)?);
        }
        Ok(result)
    }

    fn add_function(&mut self, name: Token, params_count: usize) {
        self.functions.push(FunctionData::new(name, params_count));
    }

    fn function_is_defined(&self, name: &Token) -> Option<&FunctionData> {
        self.functions
            .iter()
            .find(|function| &function.name == name)
    }

    fn declare_all_globals(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            match stmt {
                Stmt::Function { name, params, .. } => {
                    if self.function_is_defined(name).is_some() {
                        return Err(BobaError::FunctionRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        self.add_function(name.clone(), params.len());
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
            .any(|decl| &decl.name.to_string() == "main")
        {
            Err(BobaError::MainNotFound)
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, stmt: &Stmt) -> Result<LLStmt, BobaError> {
        match stmt {
            Stmt::LocalVariable {
                name,
                is_mutable,
                init,
            } => self.local_variable_decl(name, *is_mutable, init),
            Stmt::GlobalVariable { name, init } => {
                self.global_variable_decl(name, init)
            }
            Stmt::Block(stmts) => self.block_stmt(stmts, None),
            Stmt::Function {
                name,
                body,
                params,
                return_type,
            } => self.function_decl(name, body, params, return_type),
            Stmt::If {
                then,
                elze,
                condition,
            } => self.if_stmt(then, elze, condition),
            Stmt::Expression(expr) => {
                Ok(LLStmt::Expression(self.expression(expr)?))
            }
            Stmt::Print(expr) => Ok(LLStmt::Print(self.expression(expr)?)),
            Stmt::Return { name, expr } => self.return_stmt(name, expr),
            Stmt::While { condition, body } => self.while_stmt(condition, body),
        }
    }

    fn while_stmt(
        &mut self,
        condition: &Expr,
        body: &Stmt,
    ) -> Result<LLStmt, BobaError> {
        let condition = self.expression(condition)?;
        let body = Box::new(self.statement(body)?);
        Ok(LLStmt::While { condition, body })
    }

    fn return_stmt(
        &mut self,
        name: &Token,
        expr: &Expr,
    ) -> Result<LLStmt, BobaError> {
        let name = name.to_string();
        let expr = self.expression(expr)?;
        Ok(LLStmt::Return { name, expr })
    }

    fn expression(&mut self, expr: &Expr) -> Result<LLExpr, BobaError> {
        match expr {
            Expr::Number(value) => Ok(LLExpr::Number(*value)),
            Expr::Boolean(value) => Ok(LLExpr::Boolean(*value)),
            Expr::String(_value) => todo!(),
            Expr::Variable(name) => self.variable(name),
            Expr::Call { callee, args } => self.function_call(callee, args),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Assign { name, value } => self.assignment(name, value),
            Expr::Unary { oper, right } => self.unary(oper, right),
        }
    }

    fn unary(&mut self, oper: &Token, right: &Expr) -> Result<LLExpr, BobaError> {
        let right = Box::new(self.expression(right)?);
        Ok(LLExpr::Unary {
            oper: oper.kind.clone(),
            right,
        })
    }

    fn assignment(
        &mut self,
        name: &Expr,
        value: &Expr,
    ) -> Result<LLExpr, BobaError> {
        match self.expression(name)? {
            LLExpr::Variable {
                is_mutable,
                kind: Kind::Parameter(index) | Kind::LocalVariable(index),
                ..
            } => {
                if is_mutable {
                    Ok(LLExpr::Assign {
                        value: Box::new(self.expression(value)?),
                        index,
                    })
                } else {
                    Err(BobaError::AssignToImmutable(name.into()))
                }
            }
            LLExpr::Variable {
                kind: Kind::GlobalVariable,
                ..
            } => Err(BobaError::AssignToGlobalVariable(name.into())),
            _ => Err(BobaError::AssignToNonVariable(name.into())),
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
            oper: oper.kind.clone(),
            right,
        })
    }

    fn function_call(
        &mut self,
        callee: &Token,
        args: &[Expr],
    ) -> Result<LLExpr, BobaError> {
        if let Some(FunctionData { params_count, .. }) =
            self.function_is_defined(callee)
        {
            if *params_count != args.len() {
                Err(BobaError::ArgumentCountNotEqual(
                    callee.clone(),
                    *params_count,
                    args.len(),
                ))
            } else {
                let mut ll_args = vec![];
                for arg in args {
                    ll_args.push(self.expression(arg)?);
                }
                Ok(LLExpr::Call {
                    callee: callee.to_string(),
                    args: ll_args,
                })
            }
        } else {
            Err(BobaError::UndeclaredFunction(callee.clone()))
        }
    }

    fn variable(&mut self, name: &Token) -> Result<LLExpr, BobaError> {
        if let Some(info) = self.get_info(name) {
            Ok(LLExpr::Variable {
                name: name.to_string(),
                ty_pe: info.ty_pe.clone(),
                kind: info.kind.clone(),
                is_mutable: info.is_mutable,
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
        stmts: &[Stmt],
        params: Option<&[Parameter]>,
    ) -> Result<LLStmt, BobaError> {
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
        let ll_stmts = self.resolve(stmts)?;
        self.exit_scope();
        Ok(LLStmt::Block(ll_stmts))
    }

    fn if_stmt(
        &mut self,
        then: &Stmt,
        elze: &Option<Box<Stmt>>,
        condition: &Expr,
    ) -> Result<LLStmt, BobaError> {
        let condition = self.expression(condition)?;
        let then = Box::new(self.statement(then)?);
        let elze = if let Some(else_stmt) = elze {
            Some(Box::new(self.statement(else_stmt)?))
        } else {
            None
        };
        Ok(LLStmt::If {
            condition,
            then,
            elze,
        })
    }

    fn function_decl(
        &mut self,
        name: &Token,
        body: &Stmt,
        params: &[Parameter],
        _return_type: &Token,
    ) -> Result<LLStmt, BobaError> {
        let Stmt::Block(stmts) = body else {
            unreachable!("Expected function body to be a block statement.")
        };
        self.variable_index = 0;
        let body = Box::new(self.block_stmt(stmts, Some(params))?);
        let params_count = params.len() as u8;
        Ok(LLStmt::Function {
            name: name.to_string(),
            params_count,
            locals_count: self.variable_index - params_count,
            body,
        })
    }

    fn global_variable_decl(
        &mut self,
        name: &Token,
        init: &Expr,
    ) -> Result<LLStmt, BobaError> {
        let init = self.expression(init)?;
        Ok(LLStmt::GlobalVariable {
            name: name.to_string(),
            init,
        })
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
        init: &Expr,
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
            Ok(LLStmt::LocalVariable {
                init,
                ty_pe: Type::Number,
                variable_index,
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
