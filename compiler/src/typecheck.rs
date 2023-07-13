use crate::error::{BobaError, TypeError};
use crate::expr::{Expr, LLExpr};
use crate::lexer::{Span, Token, TokenType};
use crate::stmt::{Parameter, Stmt, LLStmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Type {
    Number,
    String,
    Bool,
    Unknown,
    Unit,
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
            functions: vec![],
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
        self.check_all_globals(ast);
        for stmt in ast {
            self.statement(stmt);
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
                    if init.is_constant() {
                        self.local_variable(name, init, false);
                    } else {
                        self.error(BobaError::GlobalVariableNotConst(
                            name.clone(),
                        ));
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

    fn statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            Stmt::Function { body, params, .. } => {
                self.function_decl(body, params)
            }
            Stmt::Block(stmts) => self.block(stmts, None),
            Stmt::LocalVariable {
                name,
                init,
                is_mutable,
            } => self.local_variable(name, init, *is_mutable),
            Stmt::GlobalVariable { .. } => {}
            Stmt::Expression(expr) => {
                let _ = self.expression(expr);
            }
            Stmt::While { condition, body } => self.while_stmt(condition, body),
            Stmt::Print(expr) => self.print_stmt(expr),
            Stmt::Return { name, expr } => self.return_stmt(name, expr),
        }
    }

    fn function_decl(&mut self, body: &[Stmt], params: &[Parameter]) {
        self.block(body, Some(params));
    }

    fn return_stmt(&mut self, function_name: &Token, expr: &Expr) {
        let found = self.expression(expr);
        if let Some(data) = self.function_is_defined(function_name) {
            self.error_if_ne(data.return_type, found, Token::from(expr).span);
        };
    }

    fn print_stmt(&mut self, expr: &Expr) {
        let _ty = self.expression(expr);
    }

    fn while_stmt(&mut self, condition: &Expr, body: &Stmt) {
        let cond_type = self.expression(condition);
        self.error_if_ne(Type::Bool, cond_type, Token::from(condition).span);
        self.statement(body);
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

    fn local_variable(&mut self, name: &Token, init: &Expr, is_mutable: bool) {
        if self.variable_is_declared_in_current_scope(name) {
            self.error(BobaError::VariableRedeclaration(name.clone()));
        } else {
            let init = self.expression(init);
            self.add_variable(name.clone(), Info::new(init, is_mutable));
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
            let param = param.into();
            if self.variable_is_declared_in_current_scope(&param) {
                self.error(BobaError::VariableRedeclaration(param));
            } else {
                self.add_variable(param, Info::new(*param_type, false));
            }
        }
    }

    fn block(&mut self, stmts: &[Stmt], params: Option<&[Parameter]>) {
        self.new_scope();
        if let Some(params) = params {
            self.init_parameters(params);
        };
        for stmt in stmts {
            self.statement(stmt);
        }
        self.exit_scope();
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

    fn expression(&mut self, expr: &Expr) -> LLExpr {
        match expr {
            Expr::Number { value, .. } => LLExpr::Number(*value),
            Expr::Boolean { value,.. } => LLExpr::Boolean(*value),
            Expr::String { value, .. } => LLExpr::String(value.to_owned()),
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Assign { name, value } => self.assign(name, value),
            Expr::Variable(token) => self.variable(token),
            Expr::Group(expr) => self.expression(expr),
            Expr::Unary { oper, right } => self.unary(oper, right),
            Expr::Call { callee, args } => self.function_call(callee, args),
        }
    }

    fn function_call(&mut self, function_name: &Token, args: &[Expr]) -> LLExpr {
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
                self.error_if_ne(*param_type, arg_type.to_type(), Token::from(arg).span);
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

    fn assign(&mut self, name: &Expr, value: &Expr) -> Type {
        match name {
            Expr::Variable(token) => {
                if let Some(info) = self.get_info(token) {
                    let expected = info.ty_pe;
                    if info.is_mutable {
                        let value_ty = self.expression(value);
                        self.error_if_ne(expected, value_ty, token.span);
                        value_ty
                    } else {
                        self.error(BobaError::AssignToImmutable(name.into()))
                    }
                } else {
                    self.error(BobaError::UndeclaredVariable(token.clone()))
                }
            }
            _ => self.error(BobaError::AssignToNonVariable(name.into())),
        }
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

    fn error(&mut self, error: BobaError) -> Type {
        self.errors.push(error);
        Type::Unknown
    }

    fn variable(&mut self, token: &Token) -> LLExpr {
        if let Some(Info { ty_pe, is_mutable }) = self.get_info(token) {
            LLExpr::Variable {
                name: token.to_string(),
                ty_pe: *ty_pe,
                is_mutable: *is_mutable,
            }
        } else {
            let ty_pe = self.error(BobaError::UndeclaredVariable(token.clone()));
            LLExpr::Variable {
                name: String::from(""),
                ty_pe,
                is_mutable: false,
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
