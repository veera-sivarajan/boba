use crate::error::{BobaError, TypeError};
use crate::expr::{Expr, LLExpr};
use crate::lexer::{Span, Token, TokenType};
use crate::stmt::{LLStmt, Parameter, Stmt};
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum Type {
    Number,
    String,
    Bool,
    Char,
    Unit,
    Array { ty_pe: Box<Type>, len: u16 },
}

impl Type {
    pub fn as_size(&self) -> u16 {
        match self {
            Type::Number => 4,
            Type::String => 8,
            Type::Bool | Type::Char => 1,
            Type::Unit => 0,
            Type::Array { ty_pe, len } => ty_pe.as_size() * len,
        }
    }

    pub fn count_elements(&self) -> u16 {
        if let Type::Array { ty_pe, len } = self {
            ty_pe.count_elements() * len
        } else {
            1
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }
}

fn count_args(args: &[LLExpr]) -> u16 {
    let value = args.iter().map(|arg| {
        arg.to_type().count_elements()
    }).sum();
    println!("Count args: {value}");
    value
}

fn format_type(value: &Type) -> String {
    match value {
        Type::Char => String::from("%c"),
        Type::Number => String::from("%d"),
        Type::String | Type::Bool => String::from("%s"),
        Type::Unit => String::from(" "),
        Type::Array { ty_pe, len } => {
            if *len == 0 {
                String::from("[]")
            } else {
                let mut result = String::from("[");
                for _ in 0..len - 1 {
                    result.push_str(&format_type(ty_pe));
                    result.push_str(", ");
                }
                result.push_str(&format_type(ty_pe));
                result.push(']');
                result
            }
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
    return_stmt_verified: bool,
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
            return_stmt_verified: false,
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
        return_type: &Type,
    ) {
        self.functions.push(FunctionData::new(
            name.clone(),
            params.to_vec(),
            return_type.clone(),
        ));
    }

    fn function_is_defined(&self, name: &Token) -> Option<FunctionData> {
        self.functions
            .iter()
            .find(|function| &function.name == name)
            .cloned()
    }

    fn check_all_globals(&mut self, ast: &[Stmt]) {
        let mut contains_main = false;
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
                        if name.to_string() == "main" {
                            contains_main = true;
                        };

                        if params.len() > 6 {
                            self.error(BobaError::MoreThanSixParams(
                                name.clone(),
                            ));
                        } else if name.to_string() == "main"
                            && !(*return_type == Type::Unit
                                || *return_type == Type::Number)
                        {
                            self.error(BobaError::MainReturnType(name.clone()));
                        };
                        self.add_function(name, params, return_type);
                    }
                }
                _ => continue,
            }
        }

        if !contains_main {
            self.error(BobaError::MainNotFound);
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
                name,
                body,
                params,
                return_type,
            } => self.function_decl(name, body, params, return_type),
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
            Stmt::Print { meta, args } => self.print_stmt(meta, args),
            Stmt::Return { name, expr } => self.return_stmt(name, expr),
        }
    }

    fn global_variable_decl(&mut self, name: &Token, init: &Expr) -> LLStmt {
        LLStmt::GlobalVariable {
            name: name.to_string(),
            init: self.expression(init),
        }
    }

    fn init_function_checker(&mut self) {
        self.space_for_locals = 0;
        self.return_stmt_verified = false;
    }

    fn function_decl(
        &mut self,
        name: &Token,
        body: &[Stmt],
        params: &[Parameter],
        return_type: &Type,
    ) -> LLStmt {
        self.init_function_checker();
        let body = Box::new(self.block(body, Some(params)));
        if return_type != &Type::Unit && !self.return_stmt_verified {
            self.error(BobaError::ReturnTypeNotFound(name.clone()));
        };
        let param_types: Vec<Type> =
            params.iter().map(|param| param.1.clone()).collect();
        let space_for_locals = if self.space_for_locals == 0 {
            8
        } else {
            ((((40 + self.space_for_locals) - 1) | 15) + 1) - 40
        };
        LLStmt::Function {
            name: name.to_string(),
            param_types,
            space_for_locals,
            body,
            return_type: return_type.clone(),
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
            self.return_stmt_verified = true;
        };
        LLStmt::Return {
            name: function_name.to_string(),
            value,
        }
    }

    fn replace_format(input: &str, args: &[LLExpr]) -> String {
        let mut format = String::new();
        let mut arg_iter = args.iter();
        for lexeme in input.split("{}") {
            format.push_str(lexeme);
            if let Some(arg) = arg_iter.next() {
                let specifier = format_type(&arg.to_type());
                format.push_str(&specifier);
            }
        }
        format.push_str("\\n");
        format
    }

    fn print_stmt(&mut self, meta: &Token, args: &[Expr]) -> LLStmt {
        let mut values = args.iter().map(|arg| {
            let expr = self.expression(arg);
            if expr.to_type() == Type::Unit {
                self.error(BobaError::PrintUnitType(arg.into()));
            }
            expr
        });
        if let Some(LLExpr::String(format_string)) = values.next() {
            let expected = format_string.matches("{}").count();
            let found = args.len() - 1;
            if expected != found {
                self.error(BobaError::FormatSpecifierCountNotEqual(
                    meta.clone(),
                    expected,
                    found,
                ));
                LLStmt::Error
            } else if found > 5 {
                self.error(BobaError::PrintGotMoreThanFiveArgs(meta.clone()));
                LLStmt::Error
            } else {
                let args: Vec<LLExpr> = values.collect();
                let format = TypeChecker::replace_format(&format_string, &args);
                let arg_count = count_args(&args);
                LLStmt::Print { format, args, arg_count }
            }
        } else {
            self.error(BobaError::ExpectFormatString(meta.clone()));
            LLStmt::Error
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

    fn align_stack_for(&mut self, ty_pe: &Type) {
        let align = ty_pe.as_size();
        let remainder = if align == 0 {
            align
        } else {
            self.space_for_locals % align
        };
        self.space_for_locals = if remainder == 0 {
            self.space_for_locals
        } else {
            self.space_for_locals + (align - remainder)
        };
    }

    fn update_space_for_locals(&mut self, ty_pe: &Type) -> u16 {
        self.align_stack_for(ty_pe);
        self.allocate_stack_space(ty_pe.as_size())
    }

    fn allocate_stack_space(&mut self, size: u16) -> u16 {
        self.space_for_locals += size;
        self.space_for_locals
    }

    fn local_variable(
        &mut self,
        name: &Token,
        value: &Expr,
        is_mutable: bool,
    ) -> LLStmt {
        if self.variable_is_declared_in_current_scope(name) {
            self.error(BobaError::VariableRedeclaration(name.clone()));
            LLStmt::Error
        } else {
            let init = self.expression(value);
            let variable_type = init.to_type();
            if variable_type == Type::Unit {
                self.error(BobaError::AssigningUnitType(value.into()));
            };
            let variable_index = if variable_type.is_array() {
                // arrays decay to 8 byte pointers
                self.update_space_for_locals(&Type::String)
            } else {
                self.update_space_for_locals(&variable_type)
            };
            self.add_variable(
                name.clone(),
                Info::new(
                    variable_type.clone(),
                    is_mutable,
                    Kind::LocalVariable(variable_index),
                ),
            );
            LLStmt::LocalVariable {
                ty_pe: variable_type,
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
                let parameter_index = self.update_space_for_locals(param_type);
                self.add_variable(
                    param_token,
                    Info::new(
                        param_type.clone(),
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
            Expr::Char { value, .. } => LLExpr::Char(*value),
            Expr::Number { value, .. } => LLExpr::Number(*value),
            Expr::Boolean { value, .. } => LLExpr::Boolean(*value),
            Expr::String { value, .. } => LLExpr::String(value.clone()),
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
            Expr::Array { span, elements } => self.array(span, elements),
        }
    }

    fn array(&mut self, meta: &Span, elements: &[Expr]) -> LLExpr {
        let values: Vec<LLExpr> =
            elements.iter().map(|expr| self.expression(expr)).collect();
        if values.windows(2).all(|w| w[0].to_type() == w[1].to_type()) {
            if values.is_empty() {
                LLExpr::Array {
                    ty_pe: Type::Unit,
                    elements: vec![],
                    index: self.update_space_for_locals(&Type::Unit),
                }
            } else {
                let ty_pe = values.first().unwrap().to_type();
                let index = self.update_space_for_locals(&ty_pe);
                // Subtract 1 because space is updated for first element
                let array_size = ty_pe.as_size() * (values.len() - 1) as u16;
                self.allocate_stack_space(array_size);
                LLExpr::Array {
                    index,
                    ty_pe,
                    elements: values,
                }
            }
        } else {
            self.error(BobaError::HetroArray(*meta));
            LLExpr::Array {
                ty_pe: Type::Unit,
                elements: vec![],
                index: 0,
            }
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
            self.error(BobaError::UndeclaredFunction(function_name.clone()));
            return LLExpr::Call {
                callee: String::from(""),
                args: vec![],
                ty_pe: Type::Unit,
            };
        };
        let ty_pe = if params.len() == args.len() {
            for ((_, param_type), arg) in params.iter().zip(args.iter()) {
                let arg_type = self.expression(arg);
                self.error_if_ne(
                    param_type.clone(),
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
                        self.error_if_ne(ty_pe, value_ty.clone(), token.span);
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
            };
        }
        None
    }

    fn error(&mut self, error: BobaError) -> Type {
        self.errors.push(error);
        Type::Unit
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
                name: String::new(),
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
            | TokenType::BangEqual
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
