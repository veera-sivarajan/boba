use std::fs::OpenOptions;
use std::io::prelude::*;
use std::process::Command;

fn assemble(header: String, code: String, data: String) -> std::io::Result<()> {
    let mut assembly_file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open("/home/veera/projects/boba/output/math.s")?;
    assembly_file.write_all(header.as_bytes())?;
    assembly_file.write_all(code.as_bytes())?;
    assembly_file.write_all(data.as_bytes())?;
    Command::new("gcc")
        .arg("/home/veera/projects/boba/output/math.s")
        .arg("-o")
        .arg("/home/veera/projects/boba/output/math")
        .output()
        .expect("Failed to invoke gcc.");
    Ok(())
}

fn main() -> std::io::Result<()> {
    let source = include_str!("/home/veera/projects/boba/test/math.rs");
    match compiler::compile(source.trim_end()) {
        Ok(assembly) => {
            assemble(assembly.header, assembly.code, assembly.data)
        }
        Err(err) => {
            eprintln!("{err}");
            Ok(())
        } 
    }
}
