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
    let code = fs::read_to_string(code_file.clone()).expect(&format!("Cannot find {}", code_file));

    let input = if args.len() == 4 && &args[2] == "-input" || &args[2] == "-i" {
        args[3].to_owned()
    } else {
        "".to_string()
    };

    println!("Compiling {} with input {}", code_file, input);
    let syntax = parse_code(code);
    println!("{:?}", syntax);
}
