use amethyst::config::parse_config;
use amethyst::parser::parse_code;
use amethyst::turing::TuringMachine;
use std::collections::HashSet;
use std::env;
use std::fs;

fn main() {
    println!("Amethyst - Turing Machine Programming Language");
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        panic!("No input files")
    }
    let code_file = args[1].to_owned();

    let config = parse_config(args[2..].to_vec());

    let code = fs::read_to_string(code_file.clone()).expect(&format!("Cannot find {}", code_file));

    if config.debug {
        println!("Compiling {} with the configuration {}", code_file, config);
        config.display_run();
    }
    let syntax_result = parse_code(code);
    // println!("{:?}", syntax);

    match syntax_result {
        Err(err) => println!("{}", err),
        Ok(syntax) => {
            let mut turing_machine = TuringMachine::default();
            match turing_machine.make(syntax, &config.start, &mut HashSet::new()) {
                Err(err) => println!("{}", err),
                Ok(()) => {
                    let result = turing_machine.run(&config);
                    println!("{}", result)
                }
            }
        }
    }
}
