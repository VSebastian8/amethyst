use amethyst::ast::Ast;
use amethyst::cli::{Cli, Command};
use amethyst::gem;
use amethyst::interpreter::Interpreter;
use amethyst::lsp::run_lsp_server;
use clap::Parser;

fn main() {
    let args = Cli::parse();

    let mut interpreter = Interpreter::new();

    match args.command {
        Command::Check { ref input } | Command::Run { ref input, .. } => {
            // These commands instantiate the interpreter
            if let Err(errors) = interpreter.load(input.as_str()) {
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
        Command::Check { .. } => println!("Ok, no errors found"),
        Command::List { input, all, desc } => match gem::load_ast(&input) {
            Err(err) => {
                println!("Error: {}", err);
                return;
            }
            Ok(Ast { automata, .. }) => {
                println!("Automata:");
                for automaton in automata {
                    println!("- {}", automaton.name);
                    if desc && automaton.desc != String::new() {
                        for line in automaton.desc.lines() {
                            println!("  // {}", line);
                        }
                    }
                    if all {
                        println!("  States:");
                        for state in automaton.states {
                            println!("  - {}", state.name);
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
    }
}
