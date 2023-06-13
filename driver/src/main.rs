fn main() {
    let source = include_str!("/home/veera/projects/boba/test/test.rs");
    compiler::compile(source);
}
