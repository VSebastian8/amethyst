use std::collections::HashMap;

use crate::ast::Move;
use crate::info::Error;
use crate::ir::remove_components;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fs;

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

    pub fn load_code(&mut self, code: &str) -> Result<(), Vec<Error>> {
        let mut lexer = Lexer::new(code);
        let tokens = lexer.tokenize();
        // println!(
        //     "Tokens: {:?}",
        //     tokens.iter().map(|t| t.token.clone()).collect::<Vec<_>>()
        // );

        let mut parser = Parser::new(tokens);
        let program = parser.parse();
        // println!("Program: {:?}", program);

        let errors: Vec<Error> = lexer.errors.into_iter().chain(parser.errors).collect();
        if !errors.is_empty() {
            return Err(errors);
        }

        let automata = remove_components(program)
            .map_err(|err| Error::Other(err))
            .map_err(|e| vec![e])?;
        for (automaton, repr) in automata {
            self.initial_states.insert(automaton, repr.initial_state);
            for acc in repr.accept_states {
                self.final_states.insert(acc, true);
            }
            for acc in repr.reject_states {
                self.final_states.insert(acc, false);
            }
            self.transitions.extend(repr.transitions);
        }
        Ok(())
    }

    pub fn load(&mut self, filename: &str) -> Result<(), Vec<Error>> {
        let code = fs::read_to_string(filename)
            .map_err(|e| Error::Other(e.to_string()))
            .map_err(|e| vec![e])?;
        self.load_code(code.as_str())?;
        Ok(())
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

    pub fn run(&mut self, automaton: &str, input: &str) -> Result<(), String> {
        self.set_start(automaton)?;
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
}
