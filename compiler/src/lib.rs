// mod analyzer;
// mod codegen;
mod error;
mod expr;
mod lexer;
mod parser;
mod stmt;
mod typecheck;

use crate::error::BobaError;
use crate::parser::Parser;

fn compile_helper(source: &str) -> Result<String, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    if !tokens.is_empty() {
        let mut parser = Parser::new(tokens.into_iter());
        let ast = parser.parse()?;
        typecheck::TypeChecker::new().check(&ast)?;
        Ok(String::new())
        // let ll_ast = analyzer::Analyzer::new().check(&ast)?;
        // codegen::CodeGen::new().compile(&ll_ast)
    } else {
        Err(BobaError::General(
            "Consider adding a 'main' function.".into(),
        ))
    }
}

pub fn compile(source: &str) -> Result<String, BobaError> {
    if !source.is_empty() {
        eprintln!("========Source Code===========");
        eprintln!("{source}");
        compile_helper(source)
    } else {
        Err(BobaError::General("Empty source file.".into()))
    }
}
