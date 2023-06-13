use crate::lexer::Token;
use crate::error::BobaError;
use crate::expr::Expr;
use std::iter::Peekable;

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
            todo!()
        }
        Ok(self.expressions.clone())
    }
}

