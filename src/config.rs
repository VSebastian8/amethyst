use core::fmt::Display;

pub struct Config {
    // Starting configuration of the tape
    input: String,
    // Displays the tape content from the head to the first @ symbol
    show_output: bool,
    // Displays the entire tape
    show_tape: bool,
    // Which Turing machine to run from the .myst file
    start: String,
    // Memory bound for the tape
    bound: Option<u32>,
    // Maximum number of iterations since the Turing Machine may never halt
    iterations: u32,
}

#[derive(Default)]
pub struct ConfigBuilder {
    input: Option<String>,
    show_output: Option<bool>,
    show_tape: Option<bool>,
    start: Option<String>,
    bound: Option<u32>,
    iterations: Option<u32>,
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
    pub fn set_bound(&mut self, bound: u32) {
        self.bound = Some(bound);
    }
    pub fn set_iterations(&mut self, iterations: u32) {
        self.iterations = Some(iterations);
    }
    pub fn build(self) -> Config {
        Config {
            input: self.input.unwrap_or("".to_owned()),
            show_output: self.show_output.unwrap_or(false),
            show_tape: self.show_tape.unwrap_or(false),
            start: self.start.unwrap_or("main".to_owned()),
            bound: self.bound,
            iterations: self.iterations.unwrap_or(1200),
        }
    }
}

pub fn parse_flags(args: Vec<String>) -> Config {
    let mut flags = args.iter();

    let mut configuration = ConfigBuilder::default();

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
                    .parse::<u32>()
                    .expect("Please provide a positive number for the bound"),
            ),
            "-iterations" | "-iter" | "-limit" | "-l" => configuration.set_bound(
                flags
                    .next()
                    .expect("Please provide the maximum number of tape iterations")
                    .parse::<u32>()
                    .expect("Please provide a positive number for the iterations"),
            ),
            _ => println!("Unknown flag {}", flag),
        }
    }

    configuration.build()
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
