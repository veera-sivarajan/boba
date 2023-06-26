use crate::lexer::{Token, Position, Span};
use std::fmt;

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "line {}, column {}",
            self.line_number, self.column_number
        )
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "line {}, columns {} - {}",
            self.start.line_number,
            self.start.column_number,
            self.end.column_number
        )
    }
}

#[derive(Debug)]
pub enum BobaError {
    UnterminatedString(Span),
    UnterminatedCharacter(Span),
    EmptyCharacter(Span),
    Unexpected { msg: Box<str>, span: Option<Span> },
    General(Box<str>),
    Compiler { msg: Box<str>, span: Span },
    Formatting,
    TypeError { msg: Box<str>, span: Span },
    VariableRedeclaration(Token),
    FunctionRedeclaration(Token),
    UndeclaredVariable(Token),
    UndeclaredFunction(Token),
    LocalFunction(Token),
    MainNotFound,
}

impl fmt::Display for BobaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BobaError::*;
        match self {
            UnterminatedString(span) => {
                write!(
                    f,
                    "Syntax Error: Unterminated string literal at {span}."
                )
            }
            EmptyCharacter(span) => {
                write!(
                    f,
                    "Syntax Error: Reached end of file but expected character literal at {span}."
                )
            }
            UnterminatedCharacter(span) => {
                write!(
                    f,
                    "Syntax Error: Unterminated character literal at {span}."
                )
            }
            Unexpected { msg, span } => {
                if let Some(span) = span {
                    write!(f, "Parse error: {msg} at {span}.")
                } else {
                    write!(f, "Parse error: {msg} at end of file.")
                }
            }
            General(msg) => {
                write!(f, "Error: {msg}")
            }
            Compiler { msg, span } => {
                write!(f, "Compiler Error: {msg} at {span}")
            }
            Formatting => write!(
                f,
                "Compiler Internal Error: Cannot write to assembly file."
            ),
            TypeError { msg, span } => write!(f, "TypeError: {msg} at {span}."),
            VariableRedeclaration(token) => {
                write!(f, "Cannot re-declare multiple variable with same name '{}' at {}", token, token.span)
            }
            FunctionRedeclaration(token) => {
                write!(f, "Error: Cannot re-declare function '{}' at {}.", token, token.span)
            }
            UndeclaredVariable(token) => {
                write!(f, "Error: Cannot use undeclared variable '{}' at {}", token, token.span)
            }
            UndeclaredFunction(token) => {
                write!(f, "Error: Cannot call undeclared function '{}' at {}", token, token.span)
            }
            LocalFunction(token) => {
                write!(f, "Error: Function '{}' should be declared globally at {}", token, token.span)
            }
            MainNotFound => write!(f, "Error: Consider adding a main function."),
        }
    }
}

impl From<std::fmt::Error> for BobaError {
    fn from(_value: std::fmt::Error) -> Self {
        BobaError::Formatting
    }
}

impl std::error::Error for BobaError {}
