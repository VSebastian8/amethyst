use amethyst::config::parse_flags;
use amethyst::parser::parse_code;
use std::env;
use std::fs;

fn main() {
    println!("Amethyst - Turing Machine Programming Language");
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        panic!("No input files")
    }
    let code_file = args[1].to_owned();

    let config = parse_flags(args[2..].to_vec());

    let code = fs::read_to_string(code_file.clone()).expect(&format!("Cannot find {}", code_file));

    println!("Compiling {} with the configuration {}", code_file, config);
    config.display_run();
    let syntax = parse_code(code);
    println!("{:?}", syntax);
}
