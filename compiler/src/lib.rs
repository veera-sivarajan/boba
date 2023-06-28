mod analyzer;
mod codegen;
mod error;
mod expr;
mod lexer;
mod parser;
mod stmt;

use crate::error::BobaError;

fn compile_helper(source: &str) -> Result<String, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    if !tokens.is_empty() {
        let mut parser = parser::Parser::new(tokens.into_iter());
        let ast = parser.parse()?;
        let ll_ast = analyzer::Analyzer::new().check(&ast)?;
        codegen::CodeGen::new().compile(&ll_ast)
    } else {
        Err(BobaError::General(
            "Consider adding a 'main' function.".into(),
        ))
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
