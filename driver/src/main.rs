use std::fs::OpenOptions;
use std::io::prelude::*;
use std::process::Command;

fn main() -> std::io::Result<()> {
    let source = include_str!("/home/veera/projects/boba/test/math.rs");
    match compiler::compile(source) {
        Ok(assembly) => {
            let mut assembly_file = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open("math.s")?;
            assembly_file.write_all(assembly.header.as_bytes())?;
            assembly_file.write_all(assembly.code.as_bytes())?;
            assembly_file.write_all(assembly.runtime.as_bytes())?;
            Command::new("gcc")
                .arg("math.s")
                .arg("-o")
                .arg("math")
                .output()
                .expect("Failed to invoke gcc.");
            Ok(())
        }
        Err(err) => {
            eprintln!("{err}");
            Ok(())
        } 
    }
}
