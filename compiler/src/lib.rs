mod codegen;
mod error;
mod expr;
mod lexer;
mod parser;
mod stmt;
mod inference;

use crate::codegen::Assembly;
use crate::error::BobaError;

fn compile_helper(source: &str) -> Result<Assembly, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    let mut parser = parser::Parser::new(tokens.into_iter());
    let ast = parser.parse()?;
    // println!("{ast:?}");
    // inference::infer_types(&ast);
    // inference::StaticAnalysis::new().check(&ast)?;
    let mut codegen = codegen::CodeGen::new();
    codegen.compile(&ast)
}

pub fn compile(source: &str) -> Result<Assembly, BobaError> {
    if !source.is_empty() {
        eprintln!("Compiling: {source}");
        compile_helper(source)
    } else {
        Err(BobaError::General("Empty source file.".into()))
    }
}
