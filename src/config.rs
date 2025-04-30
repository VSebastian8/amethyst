use core::fmt::Display;
use std::fs;

pub struct Config {
    // Starting configuration of the tape
    pub input: String,
    // Displays the tape content from the head to the first @ symbol
    pub show_output: bool,
    // Displays the entire tape
    pub show_tape: bool,
    // Which Turing machine to run from the .myst file
    pub start: String,
    // Memory bound for the tape
    pub bound: Option<usize>,
    // Maximum number of iterations since the Turing Machine may never halt
    pub iterations: u32,
    // Displays the current state of the Turing Machine
    pub debug: bool,
}

impl Display for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ input: {};{}{} iterations: {} ]",
            self.input,
            match (self.show_output, self.show_tape) {
                (true, true) => " show: output, tape;",
                (true, false) => " show: output;",
                (false, true) => " show: tape;",
                _ => "",
            },
            match self.bound {
                None => "".to_owned(),
                Some(bound) => format!(" bound: {};", bound),
            },
            self.iterations,
        )
    }
}

impl Config {
    pub fn display_run(&self) {
        println!(
            "Running the automata {} on input {}",
            self.start, self.input
        )
    }
}

#[derive(Default)]
pub struct ConfigBuilder {
    input: Option<String>,
    show_output: Option<bool>,
    show_tape: Option<bool>,
    start: Option<String>,
    bound: Option<usize>,
    iterations: Option<u32>,
    debug: Option<bool>,
    config_file: Option<String>,
}

impl ConfigBuilder {
    pub fn set_input(&mut self, input: String) {
        self.input = Some(input);
    }
    pub fn set_output(&mut self, show_output: bool) {
        self.show_output = Some(show_output)
    }
    pub fn set_tape(&mut self, show_tape: bool) {
        self.show_tape = Some(show_tape)
    }
    pub fn set_start(&mut self, start: String) {
        self.start = Some(start);
    }
    pub fn set_bound(&mut self, bound: usize) {
        self.bound = Some(bound);
    }
    pub fn set_iterations(&mut self, iterations: u32) {
        self.iterations = Some(iterations);
    }
    pub fn set_debug(&mut self, debug: bool) {
        self.debug = Some(debug);
    }
    pub fn set_config_file(&mut self, config_file: String) {
        self.config_file = Some(config_file);
    }
    pub fn build(self) -> Config {
        Config {
            input: self.input.unwrap_or("".to_owned()),
            show_output: self.show_output.unwrap_or(false),
            show_tape: self.show_tape.unwrap_or(false),
            start: self.start.unwrap_or("main".to_owned()),
            bound: self.bound,
            iterations: self.iterations.unwrap_or(1200),
            debug: self.debug.unwrap_or(false),
        }
    }
}

fn parse_flags(args: Vec<String>) -> ConfigBuilder {
    let mut configuration = ConfigBuilder::default();
    let mut flags = args.iter();

    while let Some(flag) = flags.next() {
        match flag.as_str() {
            "-input" | "-i" => configuration.set_input(
                flags
                    .next()
                    .expect("Please provide an input to the Turing Machine")
                    .to_owned(),
            ),
            "-output" | "-o" => configuration.set_output(true),
            "-tape" | "-t" => configuration.set_tape(true),
            "-start" | "-s" => configuration.set_start(
                flags
                    .next()
                    .expect("Please provide the name of the Turing Machine to start from")
                    .to_owned(),
            ),
            "-bound" | "-b" => configuration.set_bound(
                flags
                    .next()
                    .expect("Please provide the upper bound of the tape")
                    .parse::<usize>()
                    .expect("Please provide a positive number for the bound"),
            ),
            "-iterations" | "-iter" | "-limit" | "-l" => configuration.set_iterations(
                flags
                    .next()
                    .expect("Please provide the maximum number of tape iterations")
                    .parse::<u32>()
                    .expect("Please provide a positive number for the iterations"),
            ),
            "-debug" | "-d" => configuration.set_debug(true),
            "-config" | "-c" => configuration.set_config_file(
                flags
                    .next()
                    .expect("Please provide the path to a configuration file")
                    .to_owned(),
            ),
            _ => println!("Unknown flag {}", flag),
        }
    }
    configuration
}

pub fn merge_configs(main_config: ConfigBuilder, secondary_config: ConfigBuilder) -> ConfigBuilder {
    ConfigBuilder {
        input: main_config.input.or(secondary_config.input),
        show_output: main_config.show_output.or(secondary_config.show_output),
        show_tape: main_config.show_tape.or(secondary_config.show_tape),
        start: main_config.start.or(secondary_config.start),
        bound: main_config.bound.or(secondary_config.bound),
        iterations: main_config.iterations.or(secondary_config.iterations),
        debug: main_config.debug.or(secondary_config.debug),
        config_file: secondary_config.config_file,
    }
}

pub fn parse_config(args: Vec<String>) -> Config {
    let console_config = parse_flags(args);
    match console_config.config_file {
        None => console_config.build(),
        Some(ref config_file) => {
            let content = fs::read_to_string(config_file.clone())
                .expect(&format!("Cannot find config file {}", config_file));
            let file_config =
                parse_flags(content.split_whitespace().map(|s| s.to_owned()).collect());
            merge_configs(console_config, file_config).build()
        }
    }
}
