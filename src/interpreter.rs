use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Ast;
use crate::ast::Move;
use crate::fair::{flatten_automata, flatten_automaton};
use crate::gem;
use crate::info::ErrorInfo;

#[derive(Debug)]
pub struct Interpreter {
    initial_states: HashMap<Rc<str>, Rc<str>>,
    final_states: HashMap<Rc<str>, bool>,
    transitions: HashMap<Rc<str>, HashMap<char, (char, Move, Rc<str>)>>,
    pub state: Rc<str>,
    left: Vec<char>,
    right: Vec<char>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
            transitions: HashMap::new(),
            state: "".into(),
            left: Vec::new(),
            right: Vec::new(),
        }
    }

    pub fn load_program(&mut self, program: Ast) -> Result<(), Vec<ErrorInfo>> {
        let Ast {
            automata,
            errors: syntax_errors,
        } = program;
        let ir = flatten_automata(automata);
        let errors: Vec<_> = syntax_errors
            .iter()
            .map(|err| ErrorInfo {
                error: (*err).clone(),
                info: None,
            })
            .chain(ir.errors.into_iter())
            .collect();
        if !errors.is_empty() {
            return Err(errors);
        }
        self.initial_states.extend(ir.initial_states);
        self.final_states
            .extend(ir.accept_states.iter().map(|s| (s.clone(), true)));
        self.final_states
            .extend(ir.reject_states.iter().map(|s| (s.clone(), false)));
        self.transitions.extend(ir.transitions);
        println!("Done loading");
        Ok(())
    }

    pub fn load_automaton(
        &mut self,
        program: Ast,
        mut automaton: Rc<str>,
    ) -> Result<(), Vec<ErrorInfo>> {
        let Ast {
            automata,
            errors: syntax_errors,
        } = program;
        if automaton.as_ref() == "main"
            && automata
                .iter()
                .all(|auto| auto.name.name.as_ref() != "main")
        {
            automaton = automata[0].name.name.clone();
        }
        let ir = flatten_automaton(automata, automaton);
        let errors: Vec<_> = syntax_errors
            .iter()
            .map(|err| ErrorInfo {
                error: (*err).clone(),
                info: None,
            })
            .chain(ir.errors.into_iter())
            .collect();
        if !errors.is_empty() {
            return Err(errors);
        }
        self.initial_states.extend(ir.initial_states);
        self.final_states
            .extend(ir.accept_states.iter().map(|s| (s.clone(), true)));
        self.final_states
            .extend(ir.reject_states.iter().map(|s| (s.clone(), false)));
        self.transitions.extend(ir.transitions);
        Ok(())
    }

    pub fn load(&mut self, filename: &str, automaton: Rc<str>) -> Result<(), Vec<ErrorInfo>> {
        match gem::load_ast(filename) {
            Ok(program) => self.load_automaton(program, automaton),
            Err(err) => Err(vec![err]),
        }
    }

    pub fn load_all(&mut self, filename: &str) -> Result<(), Vec<ErrorInfo>> {
        match gem::load_ast(filename) {
            Ok(program) => self.load_program(program),
            Err(err) => Err(vec![err]),
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
            start.into()
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
