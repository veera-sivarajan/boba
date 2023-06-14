
fn main() {
    let source = include_str!("/home/veera/projects/boba/test/test.rs");
    match compiler::compile(source) {
        Ok(assembly) => {
            println!("{}", assembly.header);
            println!("{}", assembly.code);
            println!("{}", assembly.runtime);
        }
        Err(err) => eprintln!("{err}")
    }
}
