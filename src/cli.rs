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
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
    /// Verbosity level
    #[arg(long, short, default_value = "0")]
    pub verbosity: i32,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Runs the interpreter
    #[command(aliases=["r"])]
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
    #[command(aliases=["t"])]
    Test {
        /// Amethyst input file (.myst)
        input: String,
    },
    /// Lists all available automata
    #[command(aliases=["ls"])]
    List {
        /// Amethyst input file (.myst)
        input: String,
        /// List both automata and their states
        #[arg(long, short)]
        all: bool,
        /// Display descriptions
        #[arg(long, short)]
        desc: bool,
    },
    /// LSP
    #[command(aliases=["s"])]
    Server {
        /// Communicate over stdio (currently the only transport supported)
        #[arg(long)]
        stdio: bool,
    },
    /// Compile specified automaton into a linux binary
    #[command(aliases=["c"])]
    Compile {
        /// Amethyst input file (.myst)
        input: String,
        /// Turing Machine to compile
        #[arg(long, short, value_name = "AUTOMATON", default_value = "main")]
        start: String,
        /// Maximum tape size
        #[arg(long, short, default_value = "256")]
        memory: usize,
    },
}
