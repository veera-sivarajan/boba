mod lexer;
mod error;

pub fn compile(source: &str) {
    println!("{source}");
    let mut lexer = lexer::Lexer::new(source);
    let tokens = lexer.scan();
    if let Ok(token_list) = tokens {
        for t in token_list {
            println!("{}", t.kind);
        }
    } else {
        println!("Error: {tokens:?}");
    }
}
