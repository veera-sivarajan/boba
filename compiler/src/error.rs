use crate::lexer::{Position, Span, Token};
use crate::typecheck::Type;
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

#[derive(Debug, Clone)]
pub enum TypeError {
    Mismatched {
        expected: Type,
        found: Type,
        span: Span,
    },
}


impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Number => write!(f, "i64"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "bool"),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatched {
                expected,
                found,
                span,
            } => {
                write!(f, "Expected '{expected}', found '{found}' at {span}.")
            }
        }
    }
}
        

#[derive(Debug, Clone)]
pub enum BobaError {
    UnterminatedString(Span),
    UnterminatedCharacter(Span),
    EmptyCharacter(Span),
    Unexpected { msg: Box<str>, span: Option<Span> },
    General(Box<str>),
    Compiler { msg: Box<str>, span: Span },
    Formatting,
    TypeCheck(Vec<TypeError>),
    VariableRedeclaration(Token),
    FunctionRedeclaration(Token),
    UndeclaredVariable(Token),
    UndeclaredFunction(Token),
    LocalFunction(Token),
    MainNotFound,
    AssignToImmutable(Token),
    AssignToNonVariable(Token),
    AssignToGlobalVariable(Token),
    ArgumentCountNotEqual(Token, usize, usize),
}

impl fmt::Display for BobaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BobaError::*;
        match self {
            TypeCheck(errors) => {
                for err in errors {
                    writeln!(f, "{err}")?;
                }
                Ok(())
            }
            UnterminatedString(span) => {
                writeln!(
                    f,
                    "Syntax Error: Unterminated string literal at {span}."
                )
            }
            EmptyCharacter(span) => {
                writeln!(
                    f,
                    "Syntax Error: Reached end of file but expected character literal at {span}."
                )
            }
            UnterminatedCharacter(span) => {
                writeln!(
                    f,
                    "Syntax Error: Unterminated character literal at {span}."
                )
            }
            Unexpected { msg, span } => {
                if let Some(span) = span {
                    writeln!(f, "Parse error: {msg} at {span}.")
                } else {
                    writeln!(f, "Parse error: {msg} at end of file.")
                }
            }
            General(msg) => {
                writeln!(f, "Error: {msg}")
            }
            Compiler { msg, span } => {
                writeln!(f, "Compiler Error: {msg} at {span}")
            }
            Formatting => writeln!(
                f,
                "Compiler Internal Error: Cannot write to assembly file."
            ),
            VariableRedeclaration(token) => {
                writeln!(f, "Cannot re-declare multiple variable with same name '{}' at {}", token, token.span)
            }
            FunctionRedeclaration(token) => {
                writeln!(
                    f,
                    "Error: Cannot re-declare function '{}' at {}.",
                    token, token.span
                )
            }
            UndeclaredVariable(token) => {
                writeln!(
                    f,
                    "Error: Cannot use undeclared variable '{}' at {}",
                    token, token.span
                )
            }
            UndeclaredFunction(token) => {
                writeln!(
                    f,
                    "Error: Cannot call undeclared function '{}' at {}",
                    token, token.span
                )
            }
            LocalFunction(token) => {
                writeln!(
                    f,
                    "Error: Function '{}' should be declared globally at {}",
                    token, token.span
                )
            }
            MainNotFound => {
                writeln!(f, "Error: Consider adding a main function.")
            }
            AssignToImmutable(token) => {
                writeln!(
                    f,
                    "Error: Cannot assign to immutable variable '{}' at '{}'",
                    token, token.span
                )
            }
            AssignToNonVariable(token) => {
                writeln!(
                    f,
                    "Error: Attempting to assign to a non variable '{}' at {}.",
                    token, token.span
                )
            }
            AssignToGlobalVariable(token) => {
                writeln!(
                    f,
                    "Error: Global variable '{}' cannot be mutated at {}.",
                    token, token.span
                )
            }
            ArgumentCountNotEqual(
                function_name,
                expected_count,
                found_count,
            ) => {
                let expected_message = if *expected_count == 1 {
                    String::from("1 parameter")
                } else {
                    format!("{expected_count} parameters")
                };
                let found_message = if *found_count == 1 {
                    String::from("1 argument")
                } else {
                    format!("{found_count} arguments")
                };
                writeln!(
                    f,
                    "Error: Function '{function_name}' expects {expected_message} but found {found_message} at {}.",
                    function_name.span)
            }
        }
    }
}

impl From<std::fmt::Error> for BobaError {
    fn from(_value: std::fmt::Error) -> Self {
        BobaError::Formatting
    }
}

impl std::error::Error for BobaError {}
