use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;
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
            self.variable_declaration()
        } else {
            self.statement()
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

    fn variable_declaration(&mut self) -> Result<Stmt, BobaError> {
        let is_mutable = self.next_eq(TokenType::Mutable);
        let name = self.consume_identifier("Expect variable name")?;
        let init = if self.next_eq(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect semicolon")?;
        Ok(Stmt::Let {
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

    fn statement(&mut self) -> Result<Stmt, BobaError> {
        if self.next_eq(TokenType::Print) {
            self.print_stmt()
        } else if self.next_eq(TokenType::If) {
            self.if_stmt()
        } else if self.next_eq(TokenType::LeftBrace) {
            Ok(Stmt::Block(self.block_stmt()?))
        } else if self.next_eq(TokenType::Fn) {
            self.function_decl()
        } else {
            self.expression_stmt()
        }
    }

    // fn factorial(num: Number, value: Number) -> Number {
    //     ...
    // }

    fn parse_name_and_type(&mut self) -> Result<(Token, Token), BobaError> {
        let identifier = self.consume_identifier("Expect parameter name.")?;
        self.consume(TokenType::Colon, "Expect ':' after paramter name.")?;
        let id_type = self.consume_type("Expect parameter type.")?;
        Ok((identifier, id_type))
    }

    // FIXME: doesn't consider local variables
    // inside other block and if statements
    fn count_local_variables(&self, stmts: &[Stmt]) -> u8 {
        stmts
            .iter()
            .filter(|stmt| matches!(stmt, Stmt::Let { .. }))
            .count() as u8
    }

    fn function_decl(&mut self) -> Result<Stmt, BobaError> {
        let name = self.consume_identifier("Expect function name.")?;
        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;
        let mut params: Vec<(Token, Token)> = Vec::with_capacity(255);
        if !self.peek_check(TokenType::RightParen) {
            params.push(self.parse_name_and_type()?);
            while self.next_eq(TokenType::Comma) {
                params.push(self.parse_name_and_type()?);
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::Arrow, "Expect '->' after parameters.")?;
        let return_type = self.consume_type("Expect correct return type.")?;
        self.consume(TokenType::LeftBrace, "Expect '{' before function body")?;
        let body = self.block_stmt()?;
        let num_locals = self.count_local_variables(&body);
        let body = Box::new(Stmt::Block(body));
        Ok(Stmt::Function {
            name,
            params,
            return_type,
            body,
            num_locals,
        })
    }

    fn if_stmt(&mut self) -> Result<Stmt, BobaError> {
        let condition = self.expression()?;
        self.consume(
            TokenType::LeftBrace,
            "Condition should be followed by a block.",
        )?;
        let then = Box::new(Stmt::Block(self.block_stmt()?));
        let elze = if self.next_eq(TokenType::Else) {
            self.consume(
                TokenType::LeftBrace,
                "Block should follow an 'else' keyword.",
            )?;
            Some(Box::new(Stmt::Block(self.block_stmt()?)))
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then,
            elze,
        })
    }

    fn block_stmt(&mut self) -> Result<Vec<Stmt>, BobaError> {
        let mut stmts = Vec::new();
        while !self.peek_check(TokenType::RightBrace) {
            stmts.push(self.declaration()?);
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
        self.equality()
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
        if let Expr::Variable(function_name) = &callee {
            Ok(Expr::Call {
                callee: function_name.clone(),
                args,
            })
        } else {
            Err(self.error(format!("Expected a function name but found {callee:?}").as_str()))
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
