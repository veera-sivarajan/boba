use std::fs::{read_to_string, write};
use std::path::PathBuf;
use clap::Parser;

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
