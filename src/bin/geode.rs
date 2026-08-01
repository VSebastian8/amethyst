use amethyst::interpreter::Interpreter;
use amethyst::lsp::run_lsp_server;
use clap::builder::styling;
use clap::*;

const STYLES: styling::Styles = styling::Styles::styled()
    .header(styling::AnsiColor::Green.on_default().bold())
    .usage(styling::AnsiColor::Green.on_default().bold())
    .literal(styling::AnsiColor::Blue.on_default().bold())
    .placeholder(styling::AnsiColor::Cyan.on_default());

#[derive(Parser, Debug)]
#[command(name = "geode")]
#[command(version = "2.0.0")]
#[command(about = "Amethyst Compiler/Interpreter", long_about = None)]
#[command(styles = STYLES)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    /// Verbosity level
    #[arg(long, short, default_value = "0")]
    verbosity: i32,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Runs the interpreter
    Run {
        /// Amethyst input file (.myst)
        input: String,
        /// Turing Machine to execute
        #[arg(long, short, value_name = "AUTOMATON", default_value = "main")]
        start: String,
        /// Input tape content
        #[arg(
            long,
            short,
            value_name = "SYMBOLS",
            default_value = "",
            hide_default_value = true
        )]
        tape: String,
    },
    /// Checks that the input file is correct
    Check {
        /// Amethyst input file (.myst)
        input: String,
    },
    /// Lists all available automata
    List {
        /// Amethyst input file (.myst)
        input: String,
    },
    /// LSP
    Server {
        /// Communicate over stdio (currently the only transport supported)
        #[arg(long)]
        stdio: bool,
    },
}

fn main() {
    let args = Cli::parse();

    let mut interpreter = Interpreter::new();

    match args.command {
        Command::Check { ref input }
        | Command::List { ref input }
        | Command::Run { ref input, .. } => {
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
        Command::Server { .. } => {}
    }

    match args.command {
        Command::Check { .. } => println!("Ok, no errors found"),
        Command::List { .. } => {
            println!("Automata:");
            interpreter.list()
        }
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
