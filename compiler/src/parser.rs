use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::{Parameter, Stmt};
use std::iter::Peekable;

macro_rules! next_eq {
    ( $parser: ident, $( $x: expr ), *) => {
        {
            $parser.cursor.next_if(|t| $(t.kind == $x) || *)
        }
    };
}

pub struct Parser<T: Iterator<Item = Token>> {
    cursor: Peekable<T>,
    statements: Vec<Stmt>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Parser<T> {
        Parser {
            statements: Vec::with_capacity(tokens.size_hint().0),
            cursor: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, BobaError> {
        while self.cursor.peek().is_some() {
            let stmt = self.declaration()?;
            self.statements.push(stmt);
        }
        Ok(self.statements.clone())
    }

    fn next_eq(&mut self, expected: TokenType) -> bool {
        next_eq!(self, expected).is_some()
    }

    fn peek_check(&mut self, expected: TokenType) -> bool {
        self.cursor
            .peek()
            .map_or(false, |token| token.kind == expected)
    }

    fn declaration(&mut self) -> Result<Stmt, BobaError> {
        if self.next_eq(TokenType::Let) {
            self.global_variable_decl()
        } else if self.next_eq(TokenType::Fn) {
            self.function_decl()
        } else {
            Err(self.error("Expected a 'Let' or a 'fn' declaration."))
        }
    }

    fn consume_identifier(
        &mut self,
        error_msg: &str,
    ) -> Result<Token, BobaError> {
        self.cursor
            .next_if(|token| token.is_identifier())
            .ok_or(self.error(error_msg))
    }

    fn consume_type(&mut self, error_msg: &str) -> Result<Token, BobaError> {
        self.cursor
            .next_if(|token| token.is_type())
            .ok_or(self.error(error_msg))
    }

    fn global_variable_decl(&mut self) -> Result<Stmt, BobaError> {
        let name = self.consume_identifier("Expect global variable name.")?;
        self.consume(
            TokenType::Equal,
            "Global variables should be initalized at declaration.",
        )?;
        let init = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect semicolon")?;
        Ok(Stmt::GlobalVariable { name, init })
    }

    fn local_variable_decl(&mut self) -> Result<Stmt, BobaError> {
        let is_mutable = self.next_eq(TokenType::Mutable);
        let name = self.consume_identifier("Expect variable name.")?;
        self.consume(
            TokenType::Equal,
            "Variables should be initalized at declaration.",
        )?;
        let init = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect semicolon")?;
        Ok(Stmt::LocalVariable {
            name,
            is_mutable,
            init,
        })
    }

    fn consume(
        &mut self,
        expected: TokenType,
        error_msg: &str,
    ) -> Result<Token, BobaError> {
        self.cursor
            .next_if(|t| t.kind == expected)
            .ok_or(self.error(error_msg))
    }

    fn statement(&mut self, function_name: &Token) -> Result<Stmt, BobaError> {
        if self.next_eq(TokenType::Print) {
            self.print_stmt()
        } else if self.next_eq(TokenType::If) {
            self.if_stmt(function_name)
        } else if self.next_eq(TokenType::LeftBrace) {
            Ok(Stmt::Block(self.block_stmt(function_name)?))
        } else if self.next_eq(TokenType::Let) {
            self.local_variable_decl()
        } else if self.peek_check(TokenType::Fn) {
            Err(self
                .error("Functions cannot be declared within a local scope."))
        } else if self.next_eq(TokenType::Return) {
            self.return_stmt(function_name)
        } else {
            self.expression_stmt()
        }
    }

    fn return_stmt(&mut self, name: &Token) -> Result<Stmt, BobaError> {
        let expr = self.expression()?;
        self.consume(
            TokenType::Semicolon,
            "Expect semicolon at end of return statement.",
        )?;
        Ok(Stmt::Return {
            name: name.clone(),
            expr,
        })
    }

    fn parse_parameter_and_type(&mut self) -> Result<Parameter, BobaError> {
        let identifier = self.consume_identifier("Expect parameter name.")?;
        self.consume(TokenType::Colon, "Expect ':' after paramter name.")?;
        let id_type = self.consume_type("Expect parameter type.")?;
        Ok((Expr::Variable(identifier), id_type))
    }

    fn function_decl(&mut self) -> Result<Stmt, BobaError> {
        let name = self.consume_identifier("Expect function name.")?;
        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;
        let mut params: Vec<Parameter> = Vec::with_capacity(255);
        if !self.peek_check(TokenType::RightParen) {
            params.push(self.parse_parameter_and_type()?);
            while self.next_eq(TokenType::Comma) {
                params.push(self.parse_parameter_and_type()?);
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::Arrow, "Expect '->' after parameters.")?;
        let return_type = self.consume_type("Expect correct return type.")?;
        self.consume(TokenType::LeftBrace, "Expect '{' before function body")?;
        let body = self.block_stmt(&name)?;
        let body = Box::new(Stmt::Block(body));
        Ok(Stmt::Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn if_stmt(&mut self, function_name: &Token) -> Result<Stmt, BobaError> {
        let condition = self.expression()?;
        self.consume(
            TokenType::LeftBrace,
            "Condition should be followed by a block.",
        )?;
        let then = Box::new(Stmt::Block(self.block_stmt(function_name)?));
        let elze = if self.next_eq(TokenType::Else) {
            self.consume(
                TokenType::LeftBrace,
                "Block should follow an 'else' keyword.",
            )?;
            Some(Box::new(Stmt::Block(self.block_stmt(function_name)?)))
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then,
            elze,
        })
    }

    fn block_stmt(
        &mut self,
        function_name: &Token,
    ) -> Result<Vec<Stmt>, BobaError> {
        let mut stmts = Vec::new();
        while !self.peek_check(TokenType::RightBrace) {
            stmts.push(self.statement(function_name)?);
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn print_stmt(&mut self) -> Result<Stmt, BobaError> {
        self.consume(TokenType::LeftParen, "Expect opening parenthesis")?;
        if self.peek_check(TokenType::RightParen) {
            Err(self.error("Expect an expression inside print statement."))
        } else {
            let value = self.expression()?;
            self.consume(TokenType::RightParen, "Expect closing parenthesis")?;
            self.consume(
                TokenType::Semicolon,
                "Expected semicolon after print statement",
            )?;
            Ok(Stmt::Print(value))
        }
    }

    fn expression_stmt(&mut self) -> Result<Stmt, BobaError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect semicolon.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, BobaError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, BobaError> {
        let expr = self.equality()?;
        if let Some(equals) = next_eq!(self, TokenType::Equal) {
            let value = self.assignment()?;
            match expr {
                Expr::Variable(_) => Ok(Expr::Assign {
                    name: Box::new(expr),
                    value: Box::new(value),
                }),
                _ => Err(self.error(
                    format!("Expect variable name but found {expr:?}").as_str(),
                )),
            }
        } else {
            Ok(expr)
        }
    }

    fn equality(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.comparison()?;
        while let Some(oper) =
            next_eq!(self, TokenType::BangEqual, TokenType::EqualEqual)
        {
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                oper,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.term()?;
        while let Some(oper) = next_eq!(
            self,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual
        ) {
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                oper,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.factor()?;
        while let Some(oper) = next_eq!(self, TokenType::Plus, TokenType::Minus)
        {
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                oper,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.call()?;
        while let Some(oper) = next_eq!(self, TokenType::Slash, TokenType::Star)
        {
            let right = self.call()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                oper,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.primary_expression()?;
        loop {
            if self.next_eq(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, BobaError> {
        let mut args = Vec::with_capacity(255);
        if !self.peek_check(TokenType::RightParen) {
            args.push(self.expression()?);
            while self.next_eq(TokenType::Comma) {
                args.push(self.expression()?);
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        if let Expr::Variable(name) = &callee {
            Ok(Expr::Call {
                callee: name.clone(),
                args,
            })
        } else {
            Err(self.error(
                format!("Expected a function name but found {callee:?}")
                    .as_str(),
            ))
        }
    }

    fn primary_expression(&mut self) -> Result<Expr, BobaError> {
        if let Some(expr) = self.cursor.next() {
            match expr.kind {
                TokenType::Number(num) => Ok(Expr::Number(num)),
                TokenType::Identifier(_) => Ok(Expr::Variable(expr)),
                TokenType::Boolean(value) => Ok(Expr::Boolean(value)),
                TokenType::StringLiteral(lexeme) => Ok(Expr::String(lexeme)),
                _ => todo!("{}", expr.kind),
            }
        } else {
            Err(self.error("Expected a primary expression"))
        }
    }

    fn error(&mut self, message: &str) -> BobaError {
        BobaError::Unexpected {
            msg: message.into(),
            span: self.cursor.peek().map(|t| t.span),
        }
    }
}
