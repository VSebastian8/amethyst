use amethyst::ast::Ast;
use amethyst::cli::{Cli, Command};
use amethyst::compiler::read_and_compile;
use amethyst::gem;
use amethyst::interpreter::Interpreter;
use amethyst::lsp::run_lsp_server;
use clap::Parser;

fn main() {
    let args = Cli::parse();

    let mut interpreter = Interpreter::new();

    match args.command {
        Command::Test { ref input } => {
            // Instantiate the interpreter with all automata
            if let Err(errors) = interpreter.load_all(input.as_str()) {
                println!("Loading file {} failed, encountered errors:", input);
                for e in errors {
                    e.print_context();
                    println!("{}", e);
                }
                std::process::exit(1);
            }
        }
        Command::Run {
            ref input,
            ref start,
            ..
        } => {
            // Instantiate the interpreter with starting automaton
            if let Err(errors) = interpreter.load(input.as_str(), start.clone()) {
                println!("Loading file {} failed, encountered errors:", input);
                for e in errors {
                    e.print_context();
                    println!("{}", e);
                }
                std::process::exit(1);
            }
        }
        _ => {}
    }

    match args.command {
        Command::Test { .. } => println!("Ok, no errors found"),
        Command::List { input, all, desc } => match gem::load_ast(&input) {
            Err(err) => {
                println!("Error: {}", err);
                return;
            }
            Ok(Ast { automata, .. }) => {
                println!("Automata:");
                for automaton in automata {
                    println!("- {}", automaton.name.name);
                    if desc && automaton.desc != String::new() {
                        for line in automaton.desc.lines() {
                            println!("  // {}", line);
                        }
                    }
                    if all {
                        println!("  States:");
                        for state in automaton.states {
                            println!("  - {}", state.name.name);
                            if desc && state.desc != String::new() {
                                for line in state.desc.lines() {
                                    println!("      // {}", line);
                                }
                            }
                        }
                    }
                }
            }
        },
        Command::Run { start, tape, .. } => {
            if let Err(e) = interpreter.run(&start.as_str(), &tape.as_str()) {
                println!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Command::Server { .. } => {
            if let Err(err) = run_lsp_server() {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        }
        Command::Compile {
            input,
            start,
            memory,
            verbosity: _,
            debug,
        } => {
            // Compile the amethyst code and report possible errors
            if let Err(errors) = read_and_compile(&input, start, memory as u64, debug) {
                println!("Compiling file {} failed, encountered errors:", input);
                for e in errors {
                    e.print_context();
                    println!("{}", e);
                }
                std::process::exit(1);
            }
        }
    }
}
