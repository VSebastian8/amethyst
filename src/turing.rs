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

#[derive(Debug, Clone)]
pub enum Macro {
    Move(String, Move, u32),
    Override(String, Move, u32, char),
    Place(String, String),
    Shift(String, Move, u32),
}

fn into_macro(_type_name: String, name: String, macro_type: MacroType) -> Macro {
    match macro_type {
        MacroType::Move(move_symbol, number) => Macro::Move(name, move_symbol, number),
        MacroType::Override(move_symbol, number, tape_symbol) => {
            Macro::Override(name, move_symbol, number, tape_symbol)
        }
        MacroType::Place(str) => Macro::Place(name, str),
        MacroType::Shift(move_symbol, number) => Macro::Shift(name, move_symbol, number),
        _ => todo!(),
    }
}

pub struct TuringMachine {
    current_state: String,
    states: HashSet<String>,
    accept_states: HashSet<String>,
    reject_states: HashSet<String>,
    transitions: HashMap<String, HashMap<char, (String, char, Move)>>, // (state, read_symbol) -> (new_state, write_symbol, move)
    macros: HashMap<String, Box<Macro>>, // optimized macros: input state -> tape modifications -> output state
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
                        self.macros.insert(
                            name.to_string() + ".input",
                            Box::new(into_macro(
                                name.to_owned(),
                                name.to_owned(),
                                macro_type.clone(),
                            )),
                        );
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
                    AutomatonType::Macro(type_name, macro_type) => {
                        // Input state
                        self.states.insert(prefix.to_string() + ".input");
                        // Macro indexed by input_state
                        self.macros.insert(
                            prefix.to_string() + ".input",
                            Box::new(into_macro(
                                type_name.to_owned(),
                                prefix.to_owned(),
                                macro_type.clone(),
                            )),
                        );
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

    pub fn check_final(&self) -> Option<RunResult> {
        if self.accept_states.contains(&self.current_state) {
            return Some(RunResult::Accept);
        }
        if self.reject_states.contains(&self.current_state) {
            return Some(RunResult::Reject);
        }
        None
    }

    pub fn run(&mut self, config: &Config) -> RunResult {
        self.tape.initialize(config.input.to_owned());
        let mut iteration = 0;
        if config.debug {
            print!("State: {}", self.current_state);
        }

        while iteration < config.iterations {
            if let Some(result) = self.check_final() {
                return self.exit(result, config);
            }

            if let Some(macro_component) = self.macros.get(&self.current_state) {
                if let Err(result) =
                    self.apply_macro((**macro_component).clone(), &config, &mut iteration)
                {
                    return result;
                } else {
                    print!(" -> {}", self.current_state);
                }
            }
            if let Some(result) = self.check_final() {
                return self.exit(result, config);
            }
            if !self.states.contains(&self.current_state) {
                return self.exit(RunResult::Reject, config);
            }

            let read_symbol = self.tape.read();
            let (new_state, write_symbol, move_symbol) = self.get_transition(read_symbol);
            if !self.states.contains(&new_state) {
                panic!("Could not find state {}!", new_state);
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

    fn apply_macro(
        &mut self,
        macro_component: Macro,
        config: &Config,
        iteration: &mut u32,
    ) -> Result<(), RunResult> {
        match macro_component {
            Macro::Move(name, move_symbol, num) => {
                *iteration += num;
                if *iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..num).for_each(|_| self.tape.move_left()),
                    Move::Right => (0..num).for_each(|_| self.tape.move_right()),
                    Move::Neutral => {}
                }
                self.current_state = name + ".accept";
                Ok(())
            }
            Macro::Override(name, move_symbol, num, symbol) => {
                *iteration += num;
                if *iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..num).for_each(|_| {
                        self.tape.write(symbol);
                        self.tape.move_left();
                    }),
                    Move::Right => (0..num).for_each(|_| {
                        self.tape.write(symbol);
                        self.tape.move_right();
                    }),
                    Move::Neutral => self.tape.write(symbol),
                }
                self.current_state = name + ".accept";
                Ok(())
            }
            Macro::Place(name, tape_symbols) => {
                *iteration += tape_symbols.len() as u32;
                if *iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                tape_symbols.chars().for_each(|symbol| {
                    self.tape.write(symbol);
                    self.tape.move_right();
                });
                self.current_state = name + ".accept";
                Ok(())
            }
            Macro::Shift(name, move_symbol, num) => {
                *iteration += num;
                if *iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..num).for_each(|_| self.tape.shift_left()),
                    Move::Right => (0..num).for_each(|_| self.tape.shift_right()),
                    Move::Neutral => {}
                }
                self.current_state = name + ".accept";
                Ok(())
            }
        }
    }
}
