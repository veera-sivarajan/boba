mod codegen;
mod error;
mod expr;
mod analyzer;
mod lexer;
mod parser;
mod stmt;

use crate::error::BobaError;
// Option<E> to Result<(), E>
fn compile_helper(source: &str) -> Result<String, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    if !tokens.is_empty() {
        let mut parser = parser::Parser::new(tokens.into_iter());
        let mut ast = parser.parse()?;
        analyzer::Analyzer::new().check(&mut ast)?;
        let mut codegen = codegen::CodeGen::new();
        codegen.compile(&ast)
    } else {
        Err(BobaError::General("Consider adding a 'main' function.".into()))
    }
}

pub fn compile(source: &str) -> Result<String, BobaError> {
    if !source.is_empty() {
        eprintln!("Compiling: {source}");
        compile_helper(source)
    } else {
        Err(BobaError::General("Empty source file.".into()))
    }
}
