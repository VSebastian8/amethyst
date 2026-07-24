use amethyst::interpreter::Interpreter;
use std::io;
use std::io::Write;

// REPL for the interpreter
pub fn main() {
    println!("< Amethyst Interpreter v2 >");
    println!("<------------------------->");
    println!();
    let mut interpreter = Interpreter::new();
    let mut last_file: Option<String> = None;

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        // read input
        let mut input = String::new();
        if let Err(e) = io::stdin().read_line(&mut input) {
            println!("IO error: {}", e);
            break;
        }
        let input_parts: Vec<&str> = input.trim().split(" ").collect();

        match input_parts[0] {
            ":quit" | ":q" => break,
            ":help" => {
                println!("Available commands:");
                println!(":quit/:q - Exit REPL");
                println!(":list - List loaded automata");
                println!(":load/:l <file> - Load a .myst file");
                println!(":reload/:r - Reload the previous .myst file");
                println!(
                    ":run <automaton> <input> - Runs the given Turing machine on the input tape"
                );
                println!(":start <automaton> <input> - Sets up the automaton without running it");
                println!(":step - Take one transition in the Turing machine");
                println!(":tape - Shows the current state");
                println!(":tape - Shows the current tape content");
                println!(":verbosity/:v <level> - Sets the verbosity of the interpreter");
            }
            ":list" => {
                interpreter.list();
            }
            ":load" | ":l" => {
                if input_parts.len() != 2 {
                    println!("Usage: :load <filename>");
                    continue;
                }

                let filename = input_parts[1];
                if let Err(errors) = interpreter.load(filename) {
                    println!("Loading program failed, encountered errors:");
                    for e in errors {
                        e.print_context();
                        println!("{}", e);
                    }
                } else {
                    last_file = Some(filename.to_string());
                    println!("Program loaded from {}", filename);
                }
            }
            ":reload" | ":r" => match last_file {
                None => println!("No file to reload"),
                Some(ref file) => {
                    if let Err(errors) = interpreter.load(&file) {
                        println!("Loading program failed, encountered errors:");
                        for e in errors {
                            e.print_context();
                            println!("{}", e);
                        }
                    } else {
                        println!("Program reloaded from {}", file);
                    }
                }
            },
            ":run" => {
                if input_parts.len() != 3 {
                    println!("Usage: :run <automaton> <input>");
                    continue;
                }
                if let Err(e) = interpreter.run(input_parts[1], input_parts[2]) {
                    println!("Error running program: {}", e);
                }
            }
            ":start" => {
                if input_parts.len() != 3 {
                    println!("Usage: :start <automaton> <input>");
                    continue;
                }
                if let Err(e) = interpreter
                    .set_start(input_parts[1])
                    .and(interpreter.set_input(input_parts[2]))
                {
                    println!("Error running program: {}", e);
                }
            }
            ":step" => {
                interpreter.step();
            }
            ":state" => {
                println!("{}", interpreter.state);
            }
            ":tape" => {
                println!("{}", interpreter.tape());
            }
            ":verbosity" | ":v" => {
                println!("Verbosity not yet implemented");
            }
            other => {
                println!(
                    "Unrecognized comman {}, use :help to see available commands",
                    other
                );
            }
        }
    }
}
