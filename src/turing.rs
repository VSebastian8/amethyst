use std::collections::{HashMap, HashSet};

use crate::{
    config::Config,
    syntax::{AutomataType, Machine, Move, Program, StateType},
};
pub struct Tape {
    left: Vec<char>,
    right: Vec<char>,
}

impl Default for Tape {
    fn default() -> Self {
        Self {
            left: Vec::new(),
            right: Vec::new(),
        }
    }
}

impl Tape {
    pub fn read(&self) -> char {
        *self.right.last().unwrap_or(&'@')
    }
    pub fn write(&mut self, symbol: char) {
        self.right.pop();
        self.right.push(symbol);
    }
    pub fn move_left(&mut self) {
        let character = match self.left.pop() {
            Some(x) => x,
            None => '@',
        };
        self.right.push(character);
    }
    pub fn move_right(&mut self) {
        let character = match self.right.pop() {
            Some(x) => x,
            None => '@',
        };
        self.left.push(character);
    }
    fn initialize(&mut self, input: String) {
        input
            .chars()
            .rev()
            .for_each(|character| self.right.push(character));
    }
}

pub struct TuringMachine {
    current_state: String,
    states: HashSet<String>,
    accept_states: HashSet<String>,
    reject_states: HashSet<String>,
    transitions: HashMap<String, HashMap<char, (String, char, Move)>>, // (state, read_symbol) -> (new_state, write_symbol, move)
    tape: Tape,
}

impl Default for TuringMachine {
    fn default() -> Self {
        Self {
            current_state: "".to_owned(),
            states: HashSet::new(),
            accept_states: HashSet::new(),
            reject_states: HashSet::new(),
            transitions: HashMap::new(),
            tape: Tape::default(),
        }
    }
}

impl TuringMachine {
    pub fn make(syntax: Program) -> Self {
        let mut turing_machine = Self::default();
        (*syntax.automata)
            .iter()
            .for_each(|automata_type| match automata_type {
                AutomataType::Machine(name, machine) => {
                    (*machine.states)
                        .iter()
                        .for_each(|state| turing_machine.add_state(state));
                }
                AutomataType::Macro(_, _) => todo!(),
            });
        turing_machine
    }

    pub fn add_state(&mut self, state_type: &StateType) {
        match state_type {
            StateType::Accept(name) => {
                self.states.insert(name.to_owned());
                self.accept_states.insert(name.to_owned());
            }
            StateType::Reject(name) => {
                self.states.insert(name.to_owned());
                self.reject_states.insert(name.to_owned());
            }
            StateType::State(name, state) => {
                self.states.insert(name.to_owned());
                if state.initial {
                    self.current_state = name.to_owned();
                }
                (*state.transitions).iter().for_each(|transition| {
                    self.add_transition(
                        name.to_owned(),
                        transition.read_symbol,
                        transition.new_state.to_owned(),
                        transition.write_symbol,
                        transition.move_symbol,
                    )
                });
            }
        }
    }

    pub fn add_transition(
        &mut self,
        from: String,
        read_symbol: char,
        new_state: String,
        write_symbol: char,
        move_symbol: Move,
    ) {
        match self.transitions.get_mut(&from) {
            None => {
                let mut inner_map = HashMap::new();
                inner_map.insert(read_symbol, (new_state, write_symbol, move_symbol));
                self.transitions.insert(from, inner_map);
            }
            Some(inner_map) => {
                inner_map.insert(read_symbol, (new_state, write_symbol, move_symbol));
            }
        }
    }

    pub fn get_transition(&self, read_symbol: char) -> (String, char, Move) {
        match self
            .transitions
            .get(&self.current_state)
            .unwrap()
            .get(&read_symbol)
        {
            Some(tuple) => (*tuple).clone(),
            None => match self.transitions.get(&self.current_state).unwrap().get(&'_') {
                None => panic!("Sink!"),
                Some(&(ref new_state, write_symbol, move_symbol)) => {
                    if write_symbol != '_' {
                        (new_state.to_owned(), write_symbol, move_symbol)
                    } else {
                        (new_state.to_owned(), read_symbol, move_symbol)
                    }
                }
            },
        }
    }

    pub fn run(&mut self, config: Config) {
        self.tape.initialize(config.input);
        let mut iteration = 0;
        while iteration < config.iterations {
            println!("Current State {}", self.current_state);
            if self.accept_states.contains(&self.current_state) {
                println!("Accept!");
                return;
            }
            if self.reject_states.contains(&self.current_state) {
                println!("Reject!");
                return;
            }
            let read_symbol = self.tape.read();
            let (new_state, write_symbol, move_symbol) = self.get_transition(read_symbol);
            self.current_state = new_state;
            self.tape.write(write_symbol);
            match move_symbol {
                Move::Left => self.tape.move_left(),
                Move::Right => self.tape.move_right(),
                Move::Neutral => {}
            }
            iteration += 1;
        }
        println!("Turing machine surpassed the maximum number of iterations!")
    }
}
