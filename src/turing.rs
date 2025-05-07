use crate::{
    config::Config,
    syntax::{AutomatonType, MacroType, Move, Program, StateType},
    tape::Tape,
};
use core::fmt::Display;
use std::collections::{HashMap, HashSet};

pub enum RunResult {
    Accept,
    Reject,
    ExceededTime,
    ExceededMemory,
}

impl Display for RunResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RunResult::Accept => write!(f, "Accepted!"),
            RunResult::Reject => write!(f, "Rejected!"),
            RunResult::ExceededTime => write!(
                f,
                "Turing Machine exceeded the maximum number of iterations!"
            ),
            RunResult::ExceededMemory => {
                write!(f, "Turing Machine exceeded the maximum amount of memory!")
            }
        }
    }
}

pub struct TuringMachine {
    current_state: String,
    states: HashSet<String>,
    accept_states: HashSet<String>,
    reject_states: HashSet<String>,
    transitions: HashMap<String, HashMap<char, (String, char, Move)>>, // (state, read_symbol) -> (new_state, write_symbol, move)
    macros: HashMap<String, MacroType>, // optimized macros: input state -> tape modifications -> output state
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
            macros: HashMap::new(),
            tape: Tape::default(),
        }
    }
}

fn get_state_name(s: &StateType) -> &str {
    match s {
        StateType::Accept(name) => name,
        StateType::Reject(name) => name,
        StateType::State(name, _) => name,
    }
}

fn get_automaton_name(a: &AutomatonType) -> &str {
    match a {
        AutomatonType::Machine(name, _) => name,
        AutomatonType::Macro(name, _) => name,
    }
}

impl TuringMachine {
    pub fn make(
        &mut self,
        syntax: Program,
        config: &Config,
        visited: &mut HashSet<String>,
    ) -> Result<(), String> {
        let automata: HashMap<String, AutomatonType> = (*syntax.automata)
            .iter()
            .map(|automaton| (get_automaton_name(automaton).to_string(), automaton.clone()))
            .collect();

        match automata.get(&config.start) {
            None => Err(format!("Could not find start machine {}!", config.start).to_owned()),
            Some(automaton) => {
                match automaton {
                    AutomatonType::Machine(name, machine) => {
                        visited.insert(name.to_string());
                        (*machine.states)
                            .iter()
                            .for_each(|state| self.add_state(state, ""));
                        (*machine.components)
                            .iter()
                            .try_for_each(|(automaton, c_name)| {
                                self.add_component(c_name, automaton, &automata, visited)
                            })?;
                        visited.remove(name);
                    }
                    AutomatonType::Macro(name, macro_type) => {
                        // Input state of the macro
                        self.states.insert(name.to_string() + ".input");
                        self.current_state = name.to_string() + ".input";
                        // Accept state of the macro
                        self.states.insert(name.to_string() + ".accept");
                        self.accept_states.insert(name.to_string() + ".accept");
                        // Reject state of the macro
                        self.states.insert(name.to_string() + ".reject");
                        self.reject_states.insert(name.to_string() + ".reject");
                        // Macro indexed by input_state
                        self.macros
                            .insert(name.to_string() + ".input", macro_type.clone());
                    }
                }
                self.validate()
            }
        }
    }

    pub fn add_component(
        &mut self,
        prefix: &str,
        component: &String,
        automata: &HashMap<String, AutomatonType>,
        visited: &mut HashSet<String>,
    ) -> Result<(), String> {
        if visited.contains(component) {
            return Err(format!(
                "Cycles are not allowed in components. Error for component {}!",
                prefix
            ));
        }
        visited.insert(component.to_string());
        match automata.get(component) {
            None => Err(format!(
                "Could not find component {} of type {}!",
                prefix, component
            )),
            Some(automaton) => {
                match automaton {
                    AutomatonType::Machine(_, machine) => {
                        (*machine.states).iter().for_each(|state| {
                            self.add_state(state, prefix);
                        });
                        (*machine.components)
                            .iter()
                            .try_for_each(|(automaton, c_name)| {
                                self.add_component(c_name, automaton, &automata, visited)
                            })?;
                    }
                    AutomatonType::Macro(name, macro_type) => {
                        // Macro indexed by input_state
                        self.macros
                            .insert(name.to_string() + ".input", macro_type.clone());
                    }
                }
                visited.remove(component);
                Ok(())
            }
        }
    }

    pub fn add_state(&mut self, state_type: &StateType, prefix: &str) {
        let name = get_state_name(state_type);
        let top_level = prefix == "";
        let state_name = if top_level {
            name.to_owned()
        } else {
            prefix.to_owned() + "." + name
        };

        match state_type {
            StateType::Accept(_) => {
                if top_level {
                    self.states.insert(state_name.to_owned());
                    self.accept_states.insert(state_name);
                }
            }
            StateType::Reject(_) => {
                if top_level {
                    self.states.insert(state_name.to_owned());
                    self.reject_states.insert(state_name);
                }
            }
            StateType::State(_, state) => {
                self.states.insert(state_name.to_owned());
                if state.initial && top_level {
                    self.current_state = state_name.to_owned();
                }
                (*state.transitions).iter().for_each(|transition| {
                    self.add_transition(
                        state_name.to_owned(),
                        transition.read_symbol,
                        &transition.new_state,
                        transition.write_symbol,
                        transition.move_symbol,
                        prefix,
                    )
                });
            }
        }
    }

    pub fn add_transition(
        &mut self,
        from: String,
        read_symbol: char,
        new_state: &String,
        write_symbol: char,
        move_symbol: Move,
        prefix: &str,
    ) {
        let new_state_name = if prefix == "" {
            new_state.to_owned()
        } else {
            prefix.to_string() + "." + new_state
        };
        match self.transitions.get_mut(&from) {
            None => {
                let mut inner_map = HashMap::new();
                inner_map.insert(read_symbol, (new_state_name, write_symbol, move_symbol));
                self.transitions.insert(from, inner_map);
            }
            Some(inner_map) => {
                inner_map.insert(read_symbol, (new_state_name, write_symbol, move_symbol));
            }
        }
    }

    pub fn validate(&mut self) -> Result<(), String> {
        self.transitions.values().try_for_each(|inner| {
            inner.values().try_for_each(|(state, _, _)| {
                if !self.states.contains(state) {
                    Err("Undefined state ".to_owned() + state)
                } else {
                    Ok(())
                }
            })
        })
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
                None => ("reject".to_owned(), '@', Move::Neutral),
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

    pub fn run(&mut self, config: &Config) -> RunResult {
        self.tape.initialize(config.input.to_owned());
        let mut iteration = 0;
        if config.debug {
            print!("State: {}", self.current_state);
        }

        while iteration < config.iterations {
            if self.accept_states.contains(&self.current_state) {
                return self.exit(RunResult::Accept, config);
            }
            if self.reject_states.contains(&self.current_state) {
                return self.exit(RunResult::Reject, config);
            }
            let read_symbol = self.tape.read();
            let (new_state, write_symbol, move_symbol) = self.get_transition(read_symbol);
            if !self.states.contains(&new_state) {
                panic!("Unknown state {}!", new_state);
            }

            self.current_state = new_state;
            self.tape.write(write_symbol);
            match move_symbol {
                Move::Left => self.tape.move_left(),
                Move::Right => self.tape.move_right(),
                Move::Neutral => {}
            }
            if config.debug {
                print!(" -> {}", self.current_state);
            }
            match config.bound {
                Some(max_memory) => {
                    if self.tape.memory() > max_memory {
                        return self.exit(RunResult::ExceededMemory, config);
                    }
                }
                None => {}
            }
            iteration += 1;
        }

        return self.exit(RunResult::ExceededTime, config);
    }

    fn exit(&mut self, result: RunResult, config: &Config) -> RunResult {
        if config.debug {
            println!("");
        }
        if config.show_tape {
            println!("{}", self.tape);
        }
        if config.show_output {
            self.tape.output();
        }
        result
    }
}
