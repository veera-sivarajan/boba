use std::fs::OpenOptions;
use std::io::prelude::*;
use std::process::{Command, Stdio};

fn assemble(output: String) -> std::io::Result<()> {
    let mut assembly_file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open("/home/veera/projects/boba/output/math.s")?;
    assembly_file.write_all(output.as_bytes())?;
    Command::new("gcc")
        .arg("/home/veera/projects/boba/output/math.s")
        .arg("-o")
        .arg("/home/veera/projects/boba/output/math")
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to invoke gcc.");
    println!("========Program Output===========");
    Command::new("/home/veera/projects/boba/output/math")
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to run the executable.");
    Ok(())
}

fn main() -> std::io::Result<()> {
    let source = include_str!("/home/veera/projects/boba/test/math.rs");
    match compiler::compile(source.trim_end()) {
        Ok(assembly) => assemble(assembly),
        Err(err) => {
            eprintln!("{err}");
            Ok(())
        }
    }
}
