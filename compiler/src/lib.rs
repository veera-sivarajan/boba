mod lexer;
mod expr;
mod parser;
mod error;
mod codegen;
mod stmt;

use crate::error::BobaError;
use crate::codegen::Assembly;

fn compile_helper(source: &str) -> Result<Assembly, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    let mut parser = parser::Parser::new(tokens.into_iter());
    let ast = parser.parse()?;
    println!("{ast:?}");
    // let mut codegen = codegen::CodeGen::new();
    // let assembly = codegen.generate_assembly(&ast);
    Ok(Assembly::default())
}

pub fn compile(source: &str) -> Result<Assembly, BobaError> {
    if !source.is_empty() {
        eprintln!("Compiling: {source}");
        compile_helper(source) 
    } else {
        Err(BobaError::General("Empty source file.".into()))
    }
}
