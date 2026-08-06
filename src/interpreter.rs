use std::collections::HashMap;

use crate::ast::Ast;
use crate::ast::Move;
use crate::gem;
use crate::info::Error;
use crate::ir::remove_components;

pub struct Interpreter {
    initial_states: HashMap<String, String>,
    final_states: HashMap<String, bool>,
    transitions: HashMap<String, HashMap<char, (char, Move, String)>>,
    pub state: String,
    left: Vec<char>,
    right: Vec<char>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
            transitions: HashMap::new(),
            state: "".to_string(),
            left: Vec::new(),
            right: Vec::new(),
        }
    }

    pub fn load_program(&mut self, program: Ast) -> Result<(), Vec<Error>> {
        let Ast {
            automata,
            mut errors,
        } = program;
        let mut ir = remove_components(automata);
        errors.append(&mut ir.errors);
        if !errors.is_empty() {
            return Err(errors);
        }
        self.initial_states.extend(ir.initial_states);
        self.final_states
            .extend(ir.accept_states.iter().map(|s| (s.to_string(), true)));
        self.final_states
            .extend(ir.reject_states.iter().map(|s| (s.to_string(), false)));
        self.transitions.extend(ir.transitions);
        Ok(())
    }

    pub fn load(&mut self, filename: &str) -> Result<(), Vec<Error>> {
        match gem::load_ast(filename) {
            Ok(program) => self.load_program(program),
            Err(err) => Err(vec![Error::Other(err.to_string())]),
        }
    }

    pub fn step(&mut self) {
        let trans = &self.transitions[&self.state];
        let sym = self.right.pop().unwrap_or('@');
        // println!("State {} Sym {}", self.state, sym);
        if let Some((write, mov, state)) = trans.get(&sym).or(trans.get(&'_')) {
            let new_sym = if *write == '_' { sym } else { *write };
            match mov {
                Move::R => self.left.push(new_sym),
                Move::N => self.right.push(new_sym),
                Move::L => {
                    self.right.push(new_sym);
                    let left_sym = self.left.pop().unwrap_or('@');
                    self.right.push(left_sym);
                }
            }
            self.state = state.clone();
        } else {
            // Will be impossible if we insert default case _ => sink
            panic!("No transition for symbol {} and state {}", sym, self.state);
        }
    }

    pub fn set_start(&mut self, automaton: &str) -> Result<(), String> {
        if !self.initial_states.contains_key(automaton) {
            return Err(format!("Unknown automaton {}", automaton));
        }
        self.state = self.initial_states[automaton].clone();
        Ok(())
    }

    pub fn set_input(&mut self, input: &str) -> Result<(), String> {
        if let Some(x) = input
            .chars()
            .find(|x| !"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789&@".contains(*x))
        {
            return Err(format!(
                "Invalid symbol {}, use characters A-Z, 0-9, &, or @",
                x
            ));
        }
        self.left.clear();
        self.right = input.chars().rev().collect();
        Ok(())
    }

    pub fn run(&mut self, start: &str, input: &str) -> Result<(), String> {
        let automaton = if start == "main" && !self.initial_states.contains_key("main") {
            self.initial_states.keys().next().unwrap().clone()
        } else {
            start.to_string()
        };
        self.set_start(&automaton)?;
        self.set_input(input)?;
        println!("Running automaton {} on {}", automaton, self.tape());
        loop {
            self.step();
            if self.final_states.contains_key(&self.state) {
                println!(
                    "{}: reached final state {}",
                    if self.final_states[&self.state] {
                        "Accept"
                    } else {
                        "Reject"
                    },
                    self.state
                );
                break;
            }
        }
        Ok(())
    }

    pub fn tape(&self) -> String {
        format!(
            "..@{}|{}|@..",
            self.left
                .iter()
                .flat_map(|sym| ['|', *sym])
                .collect::<String>(),
            self.right
                .iter()
                .rev()
                .flat_map(|sym| ['|', *sym])
                .skip(1)
                .collect::<String>()
        )
    }

    pub fn list(&self) {
        self.initial_states
            .keys()
            .for_each(|automaton| println!("- {}", automaton));
    }

    pub fn describe() {}
}
