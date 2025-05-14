use amethyst::config::parse_config;
use amethyst::parser::parse_code;
use amethyst::turing::TuringMachine;
use colored::*;
use std::collections::HashSet;
use std::env;
use std::fs;

const VERSION: &str = "1.0.0";

fn display_help() {
    println!("geode {}", VERSION);
    println!(
        "{} {} <COMMAND>",
        "Usage:".bright_blue(),
        "geode".bright_green()
    );
    println!();
    println!("{}", "Commands:".bright_blue());
    println!(
        "   {}, {}    Run the turing machine",
        "run".bright_green(),
        "r".bright_green()
    );
    println!(
        "   {}, {}  Analyze the file for syntax errors",
        "check".bright_green(),
        "c".bright_green()
    );
    println!(
        "   {}, {}   Information about the geode command",
        "help".bright_green(),
        "h".bright_green()
    );
}

fn display_run_help() {
    println!("No input file.");
    println!(
        "{} {}",
        "Usage:".bright_blue(),
        "geode run INPUT [OPTIONS]".bright_green()
    );
    println!();
    println!("{}", "Options:".bright_blue());
    println!("      Flag                          Argument         Description           Default");
    println!(
        "   {}                     {}   Initial tape content       @",
        "-input|-i".bright_green(),
        "tape_symbols".bright_blue()
    );
    println!(
        "   {}                                   Show output                false",
        "-output|-o".bright_green(),
    );
    println!(
        "   {}                                     Show tape                  false",
        "-tape|-t".bright_green(),
    );
    println!(
        "   {}                     {}   Maximum tape memory        none",
        "-bound|-b".bright_green(),
        "cells_number".bright_blue()
    );
    println!(
        "   {}   {}   Max turing machine steps   1200",
        "-iterations|-iter|-limit|-l".bright_green(),
        "steps_number".bright_blue()
    );
    println!(
        "   {}                                    Show current state         false",
        "-debug|-d".bright_green(),
    );
    println!(
        "   {}                    {}     External flags file    ",
        "-config|-c".bright_green(),
        "config.txt".bright_blue()
    );
}

fn turing_command(args: Vec<String>, run: bool) {
    let code_file = args[1].to_owned();
    let code = match fs::read_to_string(code_file.clone()) {
        Ok(code) => code,
        Err(_) => {
            println!("{} {}", "Cannot find".red(), code_file);
            return;
        }
    };

    let config = parse_config(args[2..].to_vec());
    if config.debug {
        println!("Compiling {} with the configuration {}", code_file, config);
    }

    let syntax_result = parse_code(code);
    match syntax_result {
        Err(err) => println!("{}", err.red().bold()),
        Ok(syntax) => {
            let mut turing_machine = TuringMachine::default();
            match turing_machine.make(&syntax, &config.start, &mut HashSet::new()) {
                Err(err) => println!("{}", err.red().bold()),
                Ok(()) => {
                    if run {
                        if config.debug {
                            config.display_run();
                        }
                        let result = turing_machine.start(&config);
                        println!("{}", result)
                    } else {
                        println!("{}", "Ok!".bright_blue().bold())
                    }
                }
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        display_help();
        return;
    }
    let command = args[1].to_owned();
    match command.as_str() {
        "run" | "r" => {
            if args.len() <= 2 {
                display_run_help();
                return;
            }
            turing_command(args[1..].to_vec(), true);
        }
        "check" | "c" => {
            if args.len() <= 2 {
                println!("No input file.");
                println!(
                    "{} {}",
                    "Usage:".bright_blue(),
                    "geode check INPUT [-state|-s  machine_name]".bright_green()
                );
            }
            turing_command(args[1..].to_vec(), false);
        }
        "help" | "h" => display_help(),
        _ => {
            println!("Unrecognized subcommand {}", command.red());
            println!(
                "{} {} <COMMAND>",
                "Usage:".bright_blue(),
                "geode".bright_green()
            );
            println!();
            println!("For more information, try {}", "geode help".bright_green());
        }
    }
}
