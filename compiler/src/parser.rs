use crate::lexer::{Token, TokenType};
use crate::error::BobaError;
use crate::expr::Expr;
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
    expressions: Vec<Expr>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Parser<T> {
        Parser {
            expressions: Vec::with_capacity(tokens.size_hint().0),
            cursor: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, BobaError> {
        while self.cursor.peek().is_some() {
            let expr = self.term()?;
            self.expressions.push(expr);
        }
        Ok(self.expressions.clone())
    }

    fn term(&mut self) -> Result<Expr, BobaError> {
        let mut expr = self.factor()?;
        while let Some(oper) = next_eq!(
            self,
            TokenType::Plus,
            TokenType::Minus
        ) {
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
        while let Some(oper) = next_eq!(
            self,
            TokenType::Slash,
            TokenType::Star
        ) {
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
                _ => {
                    println!("{}", expr.kind);
                    todo!()
                },
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
