use std::fs::OpenOptions;
use std::io::prelude::*;
use std::process::{Command, Stdio};

fn assemble(output: &str) -> std::io::Result<()> {
    let mut assembly_file = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open("/home/veera/projects/boba/misc/input.s")?;
    assembly_file.write_all(output.as_bytes())?;
    Command::new("gcc")
        .arg("/home/veera/projects/boba/misc/input.s")
        .arg("-o")
        .arg("/home/veera/projects/boba/misc/input")
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to invoke gcc.");
    println!("========Program Output===========");
    Command::new("/home/veera/projects/boba/misc/input")
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect("Failed to run the executable.");
    Ok(())
}

fn main() -> std::io::Result<()> {
    let source = include_str!("/home/veera/projects/boba/misc/input.rs");
    match compiler::compile(source.trim_end()) {
        Ok(assembly) => assemble(&assembly),
        Err(err) => {
            eprint!("{err}");
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Absolute garbage code written with no regard for code quality
    // but it gets the job done

    fn execute(assembly: &str, expected_output: &str) -> bool {
        let mut assembly_file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open("/home/veera/projects/boba/test/output/temp.s")
            .unwrap();
        assembly_file.write_all(assembly.as_bytes()).unwrap();
        Command::new("gcc")
            .arg("/home/veera/projects/boba/test/output/temp.s")
            .arg("-o")
            .arg("/home/veera/projects/boba/test/output/temp")
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()
            .expect("Failed to invoke gcc.");
        let output = Command::new("/home/veera/projects/boba/test/output/temp")
            .output()
            .expect("Failed to run the executable.");
        let output = std::str::from_utf8(&output.stdout).unwrap().trim();
        output.trim() == expected_output.trim()
    }

    fn compile_and_execute(source: &str, output: &str) -> bool {
        match compiler::compile(source.trim_end()) {
            Ok(assembly) => execute(&assembly, output),
            Err(err) => output == err.to_string(),
        }
    }

    fn test_runner(file_name: &str) -> bool {
        let home_dir = "/home/veera/projects/boba/test/";
        let home_out_dir = "/home/veera/projects/boba/test/output/";
        let source_file = format!("{home_dir}{file_name}.rs");
        let output_file = format!("{home_out_dir}{file_name}.txt");
        println!("{source_file}");
        let source = std::fs::read_to_string(source_file)
            .expect("Unable to read source file.");
        let output = std::fs::read_to_string(output_file)
            .expect("Unabel to read output file.");
        compile_and_execute(&source, &output)
    }

    #[test]
    #[allow(clippy::manual_flatten)]
    fn test_all_files() {
        if let Ok(entries) = std::fs::read_dir("/home/veera/projects/boba/test")
        {
            for entry in entries {
                if let Ok(dir_entry) = entry {
                    if dir_entry.file_type().is_ok_and(|value| value.is_file())
                    {
                        let entry = dir_entry.path();
                        let stem = entry
                            .file_stem()
                            .map(|v| v.to_str().unwrap())
                            .unwrap();
                        if test_runner(stem) {
                            continue;
                        } else {
                            println!("Failed: {stem:?}.rs");
                            panic!();
                        }
                    }
                }
            }
        }
    }
}
