pub mod codegen;
mod error;
mod expr;
mod lexer;
mod parser;
mod stmt;
mod typecheck;

use crate::error::BobaError;
use crate::parser::Parser;
use crate::codegen::Assembly;

fn compile_helper(source: &str, print_ast: bool) -> Result<Assembly, BobaError> {
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan()?;
    if !tokens.is_empty() {
        let mut parser = Parser::new(tokens.into_iter());
        let ast = parser.parse()?;
        if print_ast {
            println!("{ast:#?}");
        }
        let ll_ast = typecheck::TypeChecker::new().check(&ast)?;
        Ok(codegen::CodeGen::new().compile(&ll_ast))
    } else {
        Err(BobaError::General(
            "Consider adding a 'main' function.".into(),
        ))
    }
}

pub fn compile(source: &str, print_ast: bool) -> Result<Assembly, BobaError> {
    if !source.is_empty() {
        compile_helper(source, print_ast)
    } else {
        Err(BobaError::General("Empty source file.".into()))
    }
}
