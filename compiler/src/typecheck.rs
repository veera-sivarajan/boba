use crate::error::{BobaError, TypeError};
use crate::expr::{Expr, LLExpr};
use crate::lexer::{Span, Token, TokenType};
use crate::stmt::{LLStmt, Parameter, Stmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Type {
    Number,
    String,
    Bool,
    Unknown,
    Unit,
}

impl Type {
    fn as_size(&self) -> u16 {
        match self {
            Type::Number => 8,
            Type::String => 8,
            Type::Bool => 1,
            Type::Unknown => 0,
            Type::Unit => 0,
        }
    }
}

#[derive(Clone)]
struct FunctionData {
    name: Token,
    params: Vec<Parameter>,
    return_type: Type,
}

impl FunctionData {
    fn new(name: Token, params: Vec<Parameter>, return_type: Type) -> Self {
        FunctionData {
            name,
            params,
            return_type,
        }
    }
}

pub struct TypeChecker {
    type_table: Vec<HashMap<Token, Info>>,
    functions: Vec<FunctionData>,
    errors: Vec<BobaError>,
    space_for_locals: u16,
    statements: Vec<LLStmt>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Kind {
    GlobalVariable,
    LocalVariable(u16),
    Parameter(u16),
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct Info {
    ty_pe: Type,
    is_mutable: bool,
    kind: Kind,
}

impl Info {
    fn new(ty_pe: Type, is_mutable: bool, kind: Kind) -> Self {
        Info {
            ty_pe,
            is_mutable,
            kind,
        }
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            type_table: vec![HashMap::new()],
            errors: vec![],
            functions: vec![],
            space_for_locals: 0,
            statements: vec![],
        }
    }

    fn exit(&self) -> Result<Vec<LLStmt>, BobaError> {
        if self.errors.is_empty() {
            Ok(self.statements.clone())
        } else {
            Err(BobaError::Analyzer(self.errors.clone()))
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<Vec<LLStmt>, BobaError> {
        self.check_all_globals(ast);
        for stmt in ast {
            let ll_stmt = self.statement(stmt);
            self.statements.push(ll_stmt);
        }
        self.exit()
    }

    fn add_function(
        &mut self,
        name: &Token,
        params: &[Parameter],
        return_type: Type,
    ) {
        self.functions.push(FunctionData::new(
            name.clone(),
            params.to_vec(),
            return_type,
        ));
    }

    fn function_is_defined(&self, name: &Token) -> Option<FunctionData> {
        self.functions
            .iter()
            .find(|function| &function.name == name)
            .cloned()
    }

    fn check_all_globals(&mut self, ast: &[Stmt]) {
        for stmt in ast {
            match stmt {
                Stmt::GlobalVariable { name, init } => {
                    if !init.is_constant() {
                        self.error(BobaError::GlobalVariableNotConst(
                            name.clone(),
                        ));
                    } else if self.variable_is_declared_in_current_scope(name) {
                        self.error(BobaError::VariableRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        let expr = self.expression(init);
                        self.add_variable(
                            name.clone(),
                            Info::new(
                                expr.to_type(),
                                false,
                                Kind::GlobalVariable,
                            ),
                        );
                    }
                }
                Stmt::Function {
                    name,
                    params,
                    return_type,
                    ..
                } => {
                    if self.function_is_defined(name).is_some() {
                        self.error(BobaError::FunctionRedeclaration(
                            name.clone(),
                        ));
                    } else {
                        self.add_function(name, params, *return_type);
                    }
                }
                _ => continue,
            }
        }
    }

    fn statement(&mut self, stmt: &Stmt) -> LLStmt {
        match stmt {
            Stmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            Stmt::Function {
                name, body, params, ..
            } => self.function_decl(name, body, params),
            Stmt::Block(stmts) => self.block(stmts, None),
            Stmt::LocalVariable {
                name,
                init,
                is_mutable,
            } => self.local_variable(name, init, *is_mutable),
            Stmt::GlobalVariable { name, init } => {
                self.global_variable_decl(name, init)
            }
            Stmt::Expression(expr) => LLStmt::Expression(self.expression(expr)),
            Stmt::While { condition, body } => self.while_stmt(condition, body),
            Stmt::Print(expr) => self.print_stmt(expr),
            Stmt::Return { name, expr } => self.return_stmt(name, expr),
        }
    }

    fn global_variable_decl(&mut self, name: &Token, init: &Expr) -> LLStmt {
        LLStmt::GlobalVariable {
            name: name.to_string(),
            init: self.expression(init),
        }
    }

    fn function_decl(
        &mut self,
        name: &Token,
        body: &[Stmt],
        params: &[Parameter],
    ) -> LLStmt {
        self.space_for_locals = 0;
        let body = Box::new(self.block(body, Some(params)));
        let param_sizes: Vec<u16> =
            params.iter().map(|p| p.1.as_size()).collect();
        let param_space: u16 = param_sizes.iter().sum();
        LLStmt::Function {
            name: name.to_string(),
            param_sizes,
            space_for_locals: self.space_for_locals - param_space,
            body,
        }
    }

    fn return_stmt(&mut self, function_name: &Token, expr: &Expr) -> LLStmt {
        let value = self.expression(expr);
        if let Some(data) = self.function_is_defined(function_name) {
            self.error_if_ne(
                data.return_type,
                value.to_type(),
                Token::from(expr).span,
            );
        };
        LLStmt::Return {
            name: function_name.to_string(),
            value,
        }
    }

    fn print_stmt(&mut self, expr: &Expr) -> LLStmt {
        let value = self.expression(expr);
        LLStmt::Print {
            ty_pe: value.to_type(),
            value,
        }
    }

    fn while_stmt(&mut self, cond: &Expr, body: &Stmt) -> LLStmt {
        let condition = self.expression(cond);
        self.error_if_ne(
            Type::Bool,
            condition.to_type(),
            Token::from(cond).span,
        );
        let body = Box::new(self.statement(body));
        LLStmt::While { condition, body }
    }

    fn add_variable(&mut self, name: Token, info: Info) {
        self.type_table
            .last_mut()
            .expect("Type table is empty.")
            .insert(name, info);
    }

    fn variable_is_declared_in_current_scope(&self, name: &Token) -> bool {
        self.type_table
            .last()
            .expect("No symbol table for current scope.")
            .contains_key(name)
    }

    fn update_space_for_locals(&mut self, ty_pe: Type) -> u16 {
        let value = self.space_for_locals;
        self.space_for_locals += ty_pe.as_size();
        value
    }

    fn local_variable(
        &mut self,
        name: &Token,
        init: &Expr,
        is_mutable: bool,
    ) -> LLStmt {
        if self.variable_is_declared_in_current_scope(name) {
            let ty_pe =
                self.error(BobaError::VariableRedeclaration(name.clone()));
            LLStmt::LocalVariable {
                init: LLExpr::Number(0),
                ty_pe,
                variable_index: 0,
            }
        } else {
            let init = self.expression(init);
            let variable_index = self.update_space_for_locals(init.to_type());
            self.add_variable(
                name.clone(),
                Info::new(
                    init.to_type(),
                    is_mutable,
                    Kind::LocalVariable(variable_index),
                ),
            );
            LLStmt::LocalVariable {
                ty_pe: init.to_type(),
                init,
                variable_index,
            }
        }
    }

    fn new_scope(&mut self) {
        self.type_table.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.type_table.pop();
    }

    fn init_parameters(&mut self, params: &[Parameter]) {
        for (param, param_type) in params {
            let param_token = param.into();
            if self.variable_is_declared_in_current_scope(&param_token) {
                self.error(BobaError::VariableRedeclaration(param_token));
            } else {
                let parameter_index = self.update_space_for_locals(*param_type);
                self.add_variable(
                    param_token,
                    Info::new(
                        *param_type,
                        false,
                        Kind::Parameter(parameter_index),
                    ),
                );
            }
        }
    }

    fn block(
        &mut self,
        stmts: &[Stmt],
        params: Option<&[Parameter]>,
    ) -> LLStmt {
        self.new_scope();
        if let Some(params) = params {
            self.init_parameters(params);
        };
        let ll_stmts: Vec<LLStmt> =
            stmts.iter().map(|stmt| self.statement(stmt)).collect();
        self.exit_scope();
        LLStmt::Block(ll_stmts)
    }

    fn error_if_ne(&mut self, expected: Type, found: Type, span: Span) {
        if found == Type::Unknown || expected == Type::Unknown {
            return;
        }

        if expected != found {
            self.errors
                .push(BobaError::TypeCheck(TypeError::Mismatched {
                    expected,
                    found,
                    span,
                }));
        }
    }

    fn if_stmt(
        &mut self,
        cond: &Expr,
        then: &Stmt,
        elze: &Option<Box<Stmt>>,
    ) -> LLStmt {
        let condition = self.expression(cond);
        self.error_if_ne(
            Type::Bool,
            condition.to_type(),
            Token::from(cond).span,
        );
        let then = Box::new(self.statement(then));
        let elze = elze.as_ref().map(|stmt| Box::new(self.statement(stmt)));
        LLStmt::If {
            condition,
            then,
            elze,
        }
    }

    fn expression(&mut self, expr: &Expr) -> LLExpr {
        match expr {
            Expr::Number { value, .. } => LLExpr::Number(*value),
            Expr::Boolean { value, .. } => LLExpr::Boolean(*value),
            Expr::String { value, .. } => LLExpr::String(value.to_owned()),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Assign { name, value } => self.assign(name, value),
            Expr::Variable(token) => self.variable(token),
            Expr::Group(expr) => {
                let expr = self.expression(expr);
                LLExpr::Group {
                    ty_pe: expr.to_type(),
                    value: Box::new(expr),
                }
            }
            Expr::Unary { oper, right } => self.unary(oper, right),
            Expr::Call { callee, args } => self.function_call(callee, args),
        }
    }

    fn function_call(
        &mut self,
        function_name: &Token,
        args: &[Expr],
    ) -> LLExpr {
        let Some(FunctionData {
            params,
            return_type,
            ..
        }) = self.function_is_defined(function_name)
        else {
            let ty_pe = self
                .error(BobaError::UndeclaredFunction(function_name.clone()));
            return LLExpr::Call {
                ty_pe,
                callee: String::from(""),
                args: vec![],
            };
        };
        let ty_pe = if params.len() == args.len() {
            for ((_, param_type), arg) in params.iter().zip(args.iter()) {
                let arg_type = self.expression(arg);
                self.error_if_ne(
                    *param_type,
                    arg_type.to_type(),
                    Token::from(arg).span,
                );
            }
            return_type
        } else {
            self.error(BobaError::ArgumentCountNotEqual(
                function_name.clone(),
                params.len(),
                args.len(),
            ))
        };
        LLExpr::Call {
            callee: function_name.to_string(),
            args: args.iter().map(|arg| self.expression(arg)).collect(),
            ty_pe,
        }
    }

    fn unary(&mut self, oper: &Token, right: &Expr) -> LLExpr {
        let right = self.expression(right);
        let right_type = right.to_type();
        let ty_pe = match oper.kind {
            TokenType::Bang => {
                self.error_if_ne(Type::Bool, right_type, oper.span);
                Type::Bool
            }
            TokenType::Minus => {
                self.error_if_ne(Type::Number, right_type, oper.span);
                Type::Number
            }
            _ => unreachable!(),
        };
        LLExpr::Unary {
            oper: oper.into(),
            right: Box::new(right),
            ty_pe,
        }
    }

    fn assign(&mut self, name: &Expr, value: &Expr) -> LLExpr {
        let (ty_pe, index) = match name {
            Expr::Variable(token) => {
                if let Some(Info {
                    ty_pe,
                    is_mutable,
                    kind: Kind::Parameter(index) | Kind::LocalVariable(index),
                }) = self.get_info(token)
                {
                    if is_mutable {
                        let value_ty = self.expression(value).to_type();
                        self.error_if_ne(ty_pe, value_ty, token.span);
                        (value_ty, index)
                    } else {
                        (
                            self.error(BobaError::AssignToImmutable(
                                name.into(),
                            )),
                            0,
                        )
                    }
                } else {
                    (
                        self.error(BobaError::UndeclaredVariable(
                            token.clone(),
                        )),
                        0,
                    )
                }
            }
            _ => (self.error(BobaError::AssignToNonVariable(name.into())), 0),
        };
        LLExpr::Assign {
            value: Box::new(self.expression(value)),
            index,
            ty_pe,
        }
    }

    fn get_info(&self, name: &Token) -> Option<Info> {
        for scope in self.type_table.iter().rev() {
            if scope.contains_key(name) {
                return scope.get(name).cloned();
            } else {
                continue;
            }
        }
        None
    }

    fn error(&mut self, error: BobaError) -> Type {
        self.errors.push(error);
        Type::Unknown
    }

    fn variable(&mut self, token: &Token) -> LLExpr {
        if let Some(Info {
            ty_pe,
            is_mutable,
            kind,
        }) = self.get_info(token)
        {
            LLExpr::Variable {
                name: token.to_string(),
                ty_pe,
                is_mutable,
                kind: kind.clone(),
            }
        } else {
            let ty_pe =
                self.error(BobaError::UndeclaredVariable(token.clone()));
            LLExpr::Variable {
                name: String::from(""),
                ty_pe,
                is_mutable: false,
                kind: Kind::GlobalVariable,
            }
        }
    }

    fn binary(&mut self, left: &Expr, oper: &Token, right: &Expr) -> LLExpr {
        let left = self.expression(left);
        let right = self.expression(right);
        self.error_if_ne(Type::Number, left.to_type(), oper.span);
        self.error_if_ne(Type::Number, right.to_type(), oper.span);
        let ty_pe = match oper.kind {
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
        };
        LLExpr::Binary {
            left: Box::new(left),
            oper: oper.into(),
            right: Box::new(right),
            ty_pe,
        }
    }
}
