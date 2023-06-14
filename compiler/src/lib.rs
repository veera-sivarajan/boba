mod lexer;
mod expr;
mod parser;
mod error;
mod codegen;

use crate::error::BobaError;

fn compile_helper(source: &str) -> Result<(), BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    let mut parser = parser::Parser::new(tokens.into_iter());
    let ast = parser.parse()?;
    println!("{ast:?}");
    Ok(())
}

pub fn compile(source: &str) {
    println!("{source}");
    if let Err(error) = compile_helper(source) {
        eprintln!("{error}");
    }
}
