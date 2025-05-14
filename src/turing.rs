use crate::{
    config::Config,
    result::RunResult,
    syntax::{AutomatonType, MacroType, Move, Program, StateType},
    tape::Tape,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub enum Macro {
    Move(String, Move, u32),
    Override(String, Move, u32, char),
    Place(String, String),
    Shift(String, Move, u32),
    Complement(String, TuringMachine),
    Reunion(String, Vec<TuringMachine>),
    Intersect(String, Vec<TuringMachine>),
    Chain(String, Vec<TuringMachine>),
    Repeat(String, u32, TuringMachine),
}

fn into_macro(
    _type_name: String,
    name: String,
    macro_type: MacroType,
    automata: &HashMap<String, AutomatonType>,
    visited: &mut HashSet<String>,
) -> Result<Macro, String> {
    Ok(match macro_type {
        MacroType::Move(move_symbol, number) => Macro::Move(name, move_symbol, number),
        MacroType::Override(move_symbol, number, tape_symbol) => {
            Macro::Override(name, move_symbol, number, tape_symbol)
        }
        MacroType::Place(str) => Macro::Place(name, str),
        MacroType::Shift(move_symbol, number) => Macro::Shift(name, move_symbol, number),
        MacroType::Complement(component) => {
            let mut tm = TuringMachine::default();
            tm.add_component(
                name.to_string() + &component + ".",
                &component,
                automata,
                visited,
            )?;
            tm.validate()?;
            Macro::Complement(name, tm)
        }
        MacroType::Reunion(components) => Macro::Reunion(
            name.to_owned(),
            (*components)
                .iter()
                .map(|component| {
                    let mut tm = TuringMachine::default();
                    tm.add_component(
                        name.to_string() + component + ".",
                        &component,
                        automata,
                        visited,
                    )?;
                    tm.validate()?;
                    Ok(tm)
                })
                .collect::<Result<Vec<TuringMachine>, String>>()?,
        ),
        MacroType::Intersect(components) => Macro::Intersect(
            name.to_owned(),
            (*components)
                .iter()
                .map(|component| {
                    let mut tm = TuringMachine::default();
                    tm.add_component(
                        name.to_string() + component + ".",
                        &component,
                        automata,
                        visited,
                    )?;
                    tm.validate()?;
                    Ok(tm)
                })
                .collect::<Result<Vec<TuringMachine>, String>>()?,
        ),
        MacroType::Chain(components) => Macro::Chain(
            name.to_owned(),
            (*components)
                .iter()
                .map(|component| {
                    let mut tm = TuringMachine::default();
                    tm.add_component(
                        name.to_string() + component + ".",
                        &component,
                        automata,
                        visited,
                    )?;
                    tm.validate()?;
                    Ok(tm)
                })
                .collect::<Result<Vec<TuringMachine>, String>>()?,
        ),
        MacroType::Repeat(num, component) => {
            let mut tm = TuringMachine::default();
            tm.add_component(
                name.to_string() + &component + ".",
                &component,
                automata,
                visited,
            )?;
            tm.validate()?;
            Macro::Repeat(name, num, tm)
        }
    })
}

#[derive(Debug)]
pub struct TuringMachine {
    pub initial_state: String,
    states: HashSet<String>,
    accept_states: HashSet<String>,
    reject_states: HashSet<String>,
    transitions: HashMap<String, HashMap<char, (String, char, Move)>>, // (state, read_symbol) -> (new_state, write_symbol, move)
    macros: HashMap<String, Box<Macro>>, // optimized macros: input state -> tape modifications -> output state
}

#[derive(Debug, Clone)]
pub struct TuringState {
    current_state: String,
    tape: Tape,
    iteration: u32,
}

impl Default for TuringMachine {
    fn default() -> Self {
        Self {
            initial_state: "".to_owned(),
            states: HashSet::new(),
            accept_states: HashSet::new(),
            reject_states: HashSet::new(),
            transitions: HashMap::new(),
            macros: HashMap::new(),
        }
    }
}

impl TuringState {
    pub fn new(initial_state: String, tape_symbols: String) -> Self {
        let mut tape = Tape::default();
        tape.initialize(tape_symbols);
        Self {
            current_state: initial_state,
            tape,
            iteration: 0,
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
//
impl TuringMachine {
    pub fn make(
        &mut self,
        syntax: &Program,
        start: &String,
        visited: &mut HashSet<String>,
    ) -> Result<(), String> {
        let automata: HashMap<String, AutomatonType> = (*syntax.automata)
            .iter()
            .map(|automaton| (get_automaton_name(automaton).to_string(), automaton.clone()))
            .collect();
        if !automata.contains_key(start) {
            return Err(format!(
                "Could not find start machine {}!",
                start.to_string()
            ));
        }
        self.add_component("".to_string(), start, &automata, visited)?;
        self.validate()
    }

    pub fn add_component(
        &mut self,
        prefix: String,
        component: &String,
        automata: &HashMap<String, AutomatonType>,
        visited: &mut HashSet<String>,
    ) -> Result<(), String> {
        if visited.contains(component) {
            return Err(format!(
                "Cycles are not allowed in components. Error for component {}!",
                prefix
                    .chars()
                    .take(prefix.chars().count() - 1)
                    .collect::<String>()
            ));
        }
        visited.insert(component.to_string());
        match automata.get(component) {
            None => Err(format!(
                "Could not find component {} of type {}!",
                prefix
                    .chars()
                    .take(prefix.chars().count() - 1)
                    .collect::<String>(),
                component
            )),
            Some(automaton) => {
                match automaton {
                    AutomatonType::Machine(_, machine) => {
                        (*machine.components)
                            .iter()
                            .try_for_each(|(automaton, c_name)| {
                                self.add_component(
                                    prefix.to_owned() + c_name + ".",
                                    automaton,
                                    &automata,
                                    visited,
                                )
                            })?;
                        (*machine.states).iter().for_each(|state| {
                            self.add_state(state, prefix.to_owned());
                        });
                    }
                    AutomatonType::Macro(type_name, macro_type) => {
                        // Input state of the macro
                        self.states.insert(prefix.to_string() + "input");
                        self.initial_state = prefix.to_string() + "input";
                        // Accept state of the macro
                        self.states.insert(prefix.to_string() + "accept");
                        self.accept_states.insert(prefix.to_string() + "accept");
                        // Reject state of the macro
                        self.states.insert(prefix.to_string() + "reject");
                        self.reject_states.insert(prefix.to_string() + "reject");
                        // Macro indexed by input_state
                        self.macros.insert(
                            prefix.to_string() + "input",
                            Box::new(into_macro(
                                type_name.to_owned(),
                                prefix.to_owned(),
                                macro_type.clone(),
                                automata,
                                visited,
                            )?),
                        );
                    }
                }
                visited.remove(component);
                Ok(())
            }
        }
    }

    pub fn add_state(&mut self, state_type: &StateType, prefix: String) {
        let name = get_state_name(state_type);
        let state_name = prefix.to_owned() + name;

        match state_type {
            StateType::Accept(_) => {
                self.states.insert(state_name.to_owned());
                self.accept_states.insert(state_name);
            }
            StateType::Reject(_) => {
                self.states.insert(state_name.to_owned());
                self.reject_states.insert(state_name);
            }
            StateType::State(_, state) => {
                self.states.insert(state_name.to_owned());
                self.accept_states.remove(&state_name);
                self.reject_states.remove(&state_name);

                if state.initial {
                    self.initial_state = state_name.to_owned();
                }
                (*state.transitions).iter().for_each(|transition| {
                    self.add_transition(
                        state_name.to_owned(),
                        transition.read_symbol,
                        &transition.new_state,
                        transition.write_symbol,
                        transition.move_symbol,
                        prefix.to_owned(),
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
        prefix: String,
    ) {
        let new_state_name = prefix + new_state;
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

    pub fn get_transition(&self, state: &String, read_symbol: char) -> (String, char, Move) {
        match self.transitions.get(state).unwrap().get(&read_symbol) {
            Some(tuple) => (*tuple).clone(),
            None => match self.transitions.get(state).unwrap().get(&'_') {
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

    pub fn check_final(&self, tstate: &TuringState) -> Option<RunResult> {
        if self.accept_states.contains(&tstate.current_state) {
            return Some(RunResult::Accept);
        }
        if self.reject_states.contains(&tstate.current_state) {
            return Some(RunResult::Reject);
        }
        None
    }

    pub fn start(&self, config: &Config) -> RunResult {
        let mut turing_state =
            TuringState::new(self.initial_state.to_owned(), config.input.to_owned());
        if config.debug {
            print!("State: {}", turing_state.current_state);
        }
        let result = self.run(&mut turing_state, config);
        if config.debug {
            println!("");
        }
        if config.show_tape {
            println!("{}", turing_state.tape);
        }
        if config.show_output {
            turing_state.tape.output();
        }
        result
    }

    pub fn run(&self, tstate: &mut TuringState, config: &Config) -> RunResult {
        while tstate.iteration < config.iterations {
            if let Some(result) = self.check_final(tstate) {
                return result;
            }

            if let Some(macro_component) = self.macros.get(&tstate.current_state) {
                let applied = self.apply_macro(tstate, &macro_component, &config);
                match applied {
                    Err(result) => return result,
                    Ok(()) => {
                        if config.debug {
                            print!(" -> {}", &tstate.current_state)
                        }
                    }
                }
            }
            if let Some(result) = self.check_final(tstate) {
                return result;
            }

            let read_symbol = tstate.tape.read();
            let (new_state, write_symbol, move_symbol) =
                self.get_transition(&tstate.current_state, read_symbol);
            if !self.states.contains(&new_state) {
                panic!("Could not find state {}!", new_state);
            }

            tstate.current_state = new_state;
            tstate.tape.write(write_symbol);
            match move_symbol {
                Move::Left => tstate.tape.move_left(),
                Move::Right => tstate.tape.move_right(),
                Move::Neutral => {}
            }
            if config.debug {
                print!(" -> {}", tstate.current_state);
            }
            match config.bound {
                Some(max_memory) => {
                    if tstate.tape.memory() > max_memory {
                        return RunResult::ExceededMemory;
                    }
                }
                None => {}
            }
            tstate.iteration += 1;
        }

        return RunResult::ExceededTime;
    }

    fn apply_macro(
        &self,
        tstate: &mut TuringState,
        macro_component: &Box<Macro>,
        config: &Config,
    ) -> Result<(), RunResult> {
        match macro_component.as_ref() {
            Macro::Move(name, move_symbol, num) => {
                tstate.iteration += num;
                if tstate.iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..*num).for_each(|_| tstate.tape.move_left()),
                    Move::Right => (0..*num).for_each(|_| tstate.tape.move_right()),
                    Move::Neutral => {}
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Override(name, move_symbol, num, symbol) => {
                tstate.iteration += num;
                if tstate.iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..*num).for_each(|_| {
                        tstate.tape.write(*symbol);
                        tstate.tape.move_left();
                    }),
                    Move::Right => (0..*num).for_each(|_| {
                        tstate.tape.write(*symbol);
                        tstate.tape.move_right();
                    }),
                    Move::Neutral => tstate.tape.write(*symbol),
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Place(name, tape_symbols) => {
                tstate.iteration += tape_symbols.len() as u32;
                if tstate.iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                tape_symbols.chars().for_each(|symbol| {
                    tstate.tape.write(symbol);
                    tstate.tape.move_right();
                });
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Shift(name, move_symbol, num) => {
                tstate.iteration += num;
                if tstate.iteration > config.iterations {
                    return Err(RunResult::ExceededTime);
                }
                match move_symbol {
                    Move::Left => (0..*num).for_each(|_| tstate.tape.shift_left()),
                    Move::Right => (0..*num).for_each(|_| tstate.tape.shift_right()),
                    Move::Neutral => {}
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Complement(name, tm) => {
                tstate.current_state = tm.initial_state.to_owned();
                if config.debug {
                    print!(" -> {}", &tstate.current_state)
                }
                match tm.run(tstate, config) {
                    RunResult::Accept => tstate.current_state = name.to_string() + "reject",
                    RunResult::Reject => tstate.current_state = name.to_string() + "accept",
                    r => return Err(r),
                }
                Ok(())
            }
            Macro::Reunion(name, tms) => {
                let old_tstate = tstate.clone();
                for tm in tms {
                    *tstate = old_tstate.clone();
                    tstate.current_state = tm.initial_state.to_owned();
                    if config.debug {
                        print!(" -> {}", &tstate.current_state)
                    }
                    match tm.run(tstate, config) {
                        RunResult::Accept => {
                            tstate.current_state = name.to_string() + "accept";
                            return Ok(());
                        }
                        RunResult::Reject => {}
                        r => return Err(r),
                    }
                }
                tstate.current_state = name.to_string() + "reject";
                Ok(())
            }
            Macro::Intersect(name, tms) => {
                let old_tstate = tstate.clone();
                for tm in tms {
                    *tstate = old_tstate.clone();
                    tstate.current_state = tm.initial_state.to_owned();
                    if config.debug {
                        print!(" -> {}", &tstate.current_state)
                    }
                    match tm.run(tstate, config) {
                        RunResult::Accept => {}
                        RunResult::Reject => {
                            tstate.current_state = name.to_string() + "reject";
                            return Ok(());
                        }
                        r => return Err(r),
                    }
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Chain(name, tms) => {
                for tm in tms {
                    tstate.current_state = tm.initial_state.to_owned();
                    if config.debug {
                        print!(" -> {}", &tstate.current_state)
                    }
                    match tm.run(tstate, config) {
                        RunResult::Accept => {}
                        RunResult::Reject => {
                            tstate.current_state = name.to_string() + "reject";
                            return Ok(());
                        }
                        r => return Err(r),
                    }
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
            Macro::Repeat(name, num, tm) => {
                for _ in 0..*num {
                    tstate.current_state = tm.initial_state.to_owned();
                    if config.debug {
                        print!(" -> {}", &tstate.current_state)
                    }
                    match tm.run(tstate, config) {
                        RunResult::Accept => {}
                        RunResult::Reject => {
                            tstate.current_state = name.to_string() + "reject";
                            return Ok(());
                        }
                        r => return Err(r),
                    }
                }
                tstate.current_state = name.to_string() + "accept";
                Ok(())
            }
        }
    }
}
