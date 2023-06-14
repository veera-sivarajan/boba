mod lexer;
mod expr;
mod parser;
mod error;
mod codegen;

use crate::error::BobaError;
use crate::codegen::Assembly;

fn compile_helper(source: &str) -> Result<Assembly, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    let mut parser = parser::Parser::new(tokens.into_iter());
    let ast = parser.parse()?;
    let mut codegen = codegen::CodeGen::new();
    let assembly = codegen.generate_assembly(&ast);
    Ok(assembly)
}

pub fn compile(source: &str) -> Result<Assembly, BobaError> {
    eprintln!("{source}");
    compile_helper(source) 
}
