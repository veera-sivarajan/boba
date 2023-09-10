use clap::Parser;
use std::fs::{read_to_string, write};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(version, about, long_about = None)]
struct Args {
    /// Name of the file to be compiled
    filename: String,
    /// Print the parsed AST
    #[arg(short, long)]
    print_ast: bool,
}

fn run(args: Args) -> std::io::Result<()> {
    let source = read_to_string(&args.filename)?;
    let mut file = PathBuf::from(args.filename);
    file.set_extension("s");
    let mut output = std::env::current_dir()?;
    output.push(file.file_name().unwrap());
    match compiler::compile(source.trim_end(), args.print_ast) {
        Ok(assembly) => {
            write(output, assembly)?;
            Ok(())
        }
        Err(error) => {
            eprint!("{error}");
            std::process::exit(1);
        }
    }
}

fn main() -> std::io::Result<()> {
    run(Args::parse())
}

#[cfg(test)]
mod tests {
    use std::fs::OpenOptions;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};
    use std::process::{Command, Stdio};

    fn assemble(assembly: &str, source_name: &str) -> String {
        let source_path = format!("../test/output/bin/{source_name}");
        let mut assembly_file = PathBuf::from(&source_path);
        assembly_file.set_extension("s");
        let mut executable = PathBuf::from(source_path);
        executable.set_extension("");

        // write assembly code to file
        let mut file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&assembly_file)
            .unwrap();

        file.write_all(assembly.as_bytes()).unwrap();

        // compile assembly to executable
        Command::new("gcc")
            .arg(assembly_file)
            .arg("-o")
            .arg(&executable)
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .output()
            .expect("Failed to invoke gcc.");

        // run the executable
        let output = Command::new(executable)
            .output()
            .expect("Failed to run the executable.");
        let output = std::str::from_utf8(&output.stdout).unwrap();
        output.to_string()
    }

    fn execute(input: &Path, source_name: &str) -> String {
        let source = std::fs::read_to_string(input);
        if let Ok(source) = source {
            match compiler::compile(source.trim_end(), false) {
                Ok(code) => assemble(&code, source_name),
                Err(message) => message.to_string(),
            }
        } else {
            "Cannot read source file.".into()
        }
    }

    fn check(source_name: &str, output_name: &str) -> (String, String) {
        let source = format!("../test/{source_name}");
        let output = format!("../test/output/{output_name}");
        let input = Path::new(&source);
        let output_handle = Path::new(&output);
        let result = execute(input, source_name);
        let expected = std::fs::read_to_string(output_handle)
            .unwrap_or("Cannot read output file.".into());
        (
            result.trim().to_string(),
            expected.trim().to_string(),
        )
    }

    #[test]
    fn addition() {
        let source = "addition.rs";
        let output = "addition.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn alignment() {
        let source = "alignment.rs";
        let output = "alignment.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn argument() {
        let source = "argument.rs";
        let output = "argument.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn array() {
        let source = "array.rs";
        let output = "array.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn character() {
        let source = "character.rs";
        let output = "character.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn condition_less() {
        let source = "condition-less.rs";
        let output = "condition-less.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn condition() {
        let source = "condition.rs";
        let output = "condition.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn function() {
        let source = "function.rs";
        let output = "function.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn loops() {
        let source = "loops.rs";
        let output = "loops.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn return_type() {
        let source = "return-type.rs";
        let output = "return-type.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result.to_string(), expected)
    }

    #[test]
    fn global() {
        let source = "global.rs";
        let output = "global.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn multiply() {
        let source = "multiply.rs";
        let output = "multiply.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn subscript() {
        let source = "subscript.rs";
        let output = "subscript.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn greet_with_name() {
        let source = "greet-with-name.rs";
        let output = "greet-with-name.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn three_args() {
        let source = "three-args.rs";
        let output = "three-args.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn group() {
        let source = "group.rs";
        let output = "group.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn while_loop() {
        let source = "while.rs";
        let output = "while.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn factorial() {
        let source = "factorial.rs";
        let output = "factorial.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn logic() {
        let source = "logic.rs";
        let output = "logic.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn recursive_calls() {
        let source = "recursive-calls.rs";
        let output = "recursive-calls.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn imutable() {
        let source = "imutable.rs";
        let output = "imutable.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn pass_array() {
        let source = "pass-array.rs";
        let output = "pass-array.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn type_mismatch() {
        let source = "type-mismatch.rs";
        let output = "type-mismatch.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn iter_factorial() {
        let source = "iter-factorial.rs";
        let output = "iter-factorial.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn print() {
        let source = "print.rs";
        let output = "print.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn types() {
        let source = "types.rs";
        let output = "types.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn locals() {
        let source = "locals.rs";
        let output = "locals.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn print_with_args() {
        let source = "print-with-args.rs";
        let output = "print-with-args.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn twod_array() {
        let source = "twod-array.rs";
        let output = "twod-array.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn mutate() {
        let source = "mutate.rs";
        let output = "mutate.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }

    #[test]
    fn logical() {
        let source = "logical.rs";
        let output = "logical.txt";
        let (result, expected) = check(source, output);
        assert_eq!(result, expected)
    }
}
