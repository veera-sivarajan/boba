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

    fn variable_declaration(&mut self) -> Result<Stmt, BobaError> {
        let is_mutable = self.next_eq(TokenType::Mutable);
        let name = self.consume_identifier("Expect variable name")?;
        let init = if self.next_eq(TokenType::Equal) {
            Some(self.term()?)
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
        } else {
            self.expression_stmt()
        }
    }

    // FIXME: panics when there is no expression
    // inside print function
    fn print_stmt(&mut self) -> Result<Stmt, BobaError> {
        self.consume(TokenType::LeftParen, "Expect opening parenthesis")?;
        let value = self.term()?;
        self.consume(TokenType::RightParen, "Expect closing parenthesis")?;
        self.consume(TokenType::Semicolon, "Expected semicolon after print statement")?;
        Ok(Stmt::Print(value))
    }

    fn expression_stmt(&mut self) -> Result<Stmt, BobaError> {
        let expr = self.term()?;
        self.consume(TokenType::Semicolon, "Expect semicolon.")?;
        Ok(Stmt::Expression(expr))
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
        let mut expr = self.primary_expression()?;
        while let Some(oper) = next_eq!(self, TokenType::Slash, TokenType::Star)
        {
            let right = self.primary_expression()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                oper,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn primary_expression(&mut self) -> Result<Expr, BobaError> {
        if let Some(expr) = self.cursor.next() {
            match expr.kind {
                TokenType::Number(num) => Ok(Expr::Number(num)),
                TokenType::Identifier(_) => Ok(Expr::Variable(expr)),
                _ => {
                    println!("{}", expr.kind);
                    todo!()
                }
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
