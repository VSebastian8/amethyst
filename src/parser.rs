// Call Haskell Parser through FFI
// Construct the resulting syntax
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use crate::syntax::*;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn amethyst_parser(code: *const c_char) -> *mut i32;
    fn free_result(result_ptr: *mut i32);
    // Move function
    fn move_type(move_ptr: *mut i32) -> i32;
    // Transition functions
    fn transition_read_symbol(transition_ptr: *mut i32) -> c_char;
    fn transition_write_symbol(transition_ptr: *mut i32) -> c_char;
    fn transition_move_symbol(transition_ptr: *mut i32) -> *mut i32;
    fn transition_new_state(transition_ptr: *mut i32) -> *mut c_char;
    // State functions
    fn state_type(state_ptr: *mut i32) -> i32;
    fn state_name(state_ptr: *mut i32) -> *mut c_char;
    fn state_is_initial(state_ptr: *mut i32) -> bool;
    fn state_tr_len(state_ptr: *mut i32) -> i32;
    fn state_transitions(state_ptr: *mut i32) -> *mut i32;
    fn state_transition_i(transitions_ptr: *mut i32, i: i32) -> *mut i32;
    // Macro functions
    fn macro_type(macro_ptr: *mut i32) -> i32;
    fn macro_string(macro_ptr: *mut i32) -> *mut c_char;
    fn macro_number(macro_ptr: *mut i32) -> i32;
    fn macro_move(macro_ptr: *mut i32) -> *mut i32;
    fn macro_symbol(macro_ptr: *mut i32) -> c_char;
    fn macro_list_len(macro_ptr: *mut i32) -> i32;
    fn macro_list(macro_ptr: *mut i32) -> *mut i32;
    fn macro_list_i(macro_ptr: *mut i32, i: i32) -> *mut c_char;
    // Machine functions
    fn machine_components_len(machine_ptr: *mut i32) -> i32;
    fn machine_components(machine_ptr: *mut i32) -> *mut i32;
    fn machine_components_i_first(components_ptr: *mut i32, i: i32) -> *mut c_char;
    fn machine_components_i_second(components_ptr: *mut i32, i: i32) -> *mut c_char;
    fn machine_states_len(machine_ptr: *mut i32) -> i32;
    fn machine_states(machine_ptr: *mut i32) -> *mut i32;
    fn machine_states_i(states: *mut i32, i: i32) -> *mut i32;
    // Automata functions
    fn automata_type(automata_ptr: *mut i32) -> i32;
    fn automata_name(automata_ptr: *mut i32) -> *mut c_char;
    fn automata_machine(automata_ptr: *mut i32) -> *mut i32;
    fn automata_macro(automata_ptr: *mut i32) -> *mut i32;
    // Result functions
    fn result_type(result_ptr: *mut i32) -> i32;
    fn result_program_len(program_ptr: *mut i32) -> i32;
    fn program_automata(program_ptr: *mut i32) -> *mut i32;
    fn program_automata_i(automata_ptr: *mut i32, i: i32) -> *mut i32;
    fn error_string(error_ptr: *mut i32) -> *mut c_char;
    fn error_line(error_ptr: *mut i32) -> i32;
    fn error_column(error_ptr: *mut i32) -> i32;
}

fn parse_move(move_ptr: *mut i32) -> Move {
    unsafe {
        match move_type(move_ptr) {
            0 => Move::Left,
            1 => Move::Right,
            2 => Move::Neutral,
            _ => panic!("Unexpected move type"),
        }
    }
}

pub fn parse_transition(transition_ptr: *mut i32) -> Transition {
    unsafe {
        let read_symbol = transition_read_symbol(transition_ptr) as u8 as char;
        let write_symbol = transition_write_symbol(transition_ptr) as u8 as char;
        // Get the move as a pointer and then find which move it is
        let move_ptr = transition_move_symbol(transition_ptr);
        let move_symbol = parse_move(move_ptr);
        // Convert from CString to Rust String
        let new_state = CStr::from_ptr(transition_new_state(transition_ptr))
            .to_str()
            .expect("Error converting CString to String")
            .to_owned();

        Transition {
            read_symbol,
            write_symbol,
            move_symbol,
            new_state,
        }
    }
}

pub fn parse_state(state_ptr: *mut i32) -> StateType {
    unsafe {
        let state_name = CStr::from_ptr(state_name(state_ptr))
            .to_str()
            .expect("Error converting CString to String")
            .to_owned();
        match state_type(state_ptr) {
            0 => StateType::Accept(state_name),
            1 => StateType::Reject(state_name),
            2 => {
                let initial = state_is_initial(state_ptr);
                let transitions_len = state_tr_len(state_ptr);
                let transitions_ptr = state_transitions(state_ptr);
                // Get transitions one by one
                let transitions = Box::new(
                    (0..transitions_len)
                        .map(|i| state_transition_i(transitions_ptr, i))
                        .map(parse_transition)
                        .collect(),
                );
                StateType::State(
                    state_name,
                    State {
                        initial,
                        transitions,
                    },
                )
            }
            _ => panic!("Unexpected state type"),
        }
    }
}

pub fn parse_macro(macro_ptr: *mut i32) -> MacroType {
    unsafe {
        match macro_type(macro_ptr) {
            0 => {
                let macro_automata = CStr::from_ptr(macro_string(macro_ptr))
                    .to_str()
                    .expect("Error converting CString to String")
                    .to_owned();
                MacroType::Complement(macro_automata)
            }
            1 => {
                let automata_list_len = macro_list_len(macro_ptr);
                let automata_list_ptr = macro_list(macro_ptr);
                // Get transitions one by one
                let automata_list = Box::new(
                    (0..automata_list_len)
                        .map(|i| macro_list_i(automata_list_ptr, i))
                        .map(|s| {
                            CStr::from_ptr(s)
                                .to_str()
                                .expect("Error converting CString to String")
                                .to_owned()
                        })
                        .collect(),
                );
                MacroType::Intersect(automata_list)
            }
            2 => {
                let automata_list_len = macro_list_len(macro_ptr);
                let automata_list_ptr = macro_list(macro_ptr);
                // Get transitions one by one
                let automata_list = Box::new(
                    (0..automata_list_len)
                        .map(|i| macro_list_i(automata_list_ptr, i))
                        .map(|s| {
                            CStr::from_ptr(s)
                                .to_str()
                                .expect("Error converting CString to String")
                                .to_owned()
                        })
                        .collect(),
                );
                MacroType::Reunion(automata_list)
            }
            3 => {
                let automata_list_len = macro_list_len(macro_ptr);
                let automata_list_ptr = macro_list(macro_ptr);
                // Get transitions one by one
                let automata_list = Box::new(
                    (0..automata_list_len)
                        .map(|i| macro_list_i(automata_list_ptr, i))
                        .map(|s| {
                            CStr::from_ptr(s)
                                .to_str()
                                .expect("Error converting CString to String")
                                .to_owned()
                        })
                        .collect(),
                );
                MacroType::Chain(automata_list)
            }
            4 => {
                let number = macro_number(macro_ptr);
                let automata = CStr::from_ptr(macro_string(macro_ptr))
                    .to_str()
                    .expect("Error converting CString to String")
                    .to_owned();
                MacroType::Repeat(number, automata)
            }
            5 => {
                let number = macro_number(macro_ptr);
                let move_symbol = parse_move(macro_move(macro_ptr));
                MacroType::Move(move_symbol, number)
            }
            6 => {
                let number = macro_number(macro_ptr);
                let move_symbol = parse_move(macro_move(macro_ptr));
                let override_symbol = macro_symbol(macro_ptr) as u8 as char;
                MacroType::Override(move_symbol, number, override_symbol)
            }
            7 => {
                let macro_text = CStr::from_ptr(macro_string(macro_ptr))
                    .to_str()
                    .expect("Error converting CString to String")
                    .to_owned();
                MacroType::Place(macro_text)
            }
            8 => {
                let number = macro_number(macro_ptr);
                let move_symbol = parse_move(macro_move(macro_ptr));
                MacroType::Shift(move_symbol, number)
            }
            _ => panic!("Unexpected macro keyword type!"),
        }
    }
}

pub fn parse_machine(machine_ptr: *mut i32) -> Machine {
    unsafe {
        let comp_len = machine_components_len(machine_ptr);
        let comp_list = machine_components(machine_ptr);
        let components = Box::new(
            (0..comp_len)
                .map(|i| {
                    (
                        machine_components_i_first(comp_list, i),
                        machine_components_i_second(comp_list, i),
                    )
                })
                .map(|(s1, s2)| {
                    (
                        CStr::from_ptr(s1)
                            .to_str()
                            .expect("Error converting CString to String")
                            .to_owned(),
                        CStr::from_ptr(s2)
                            .to_str()
                            .expect("Error converting CString to String")
                            .to_owned(),
                    )
                })
                .collect(),
        );
        let states_len = machine_states_len(machine_ptr);
        let states_list = machine_states(machine_ptr);
        let states = Box::new(
            (0..states_len)
                .map(|i| machine_states_i(states_list, i))
                .map(parse_state)
                .collect(),
        );
        Machine { components, states }
    }
}

pub fn parse_automata(automata_ptr: *mut i32) -> AutomataType {
    unsafe {
        let automata_name = CStr::from_ptr(automata_name(automata_ptr))
            .to_str()
            .expect("Error converting CString to String")
            .to_owned();
        match automata_type(automata_ptr) {
            0 => {
                AutomataType::Machine(automata_name, parse_machine(automata_machine(automata_ptr)))
            }

            1 => AutomataType::Macro(automata_name, parse_macro(automata_macro(automata_ptr))),
            _ => panic!("Unexpected automata type"),
        }
    }
}

fn parse_program(program_ptr: *mut i32) -> Program {
    unsafe {
        let automata_len = result_program_len(program_ptr);
        let automata_list = program_automata(program_ptr);
        let automata = Box::new(
            (0..automata_len)
                .map(|i| program_automata_i(automata_list, i))
                .map(parse_automata)
                .collect(),
        );
        Program { automata }
    }
}

fn parse_error(error_ptr: *mut i32) -> String {
    unsafe {
        let error_string = CStr::from_ptr(error_string(error_ptr))
            .to_str()
            .expect("Error converting CString to String")
            .to_owned();
        let line = error_line(error_ptr);
        let column = error_column(error_ptr);
        return format!("Error: {} at line {} column {}", error_string, line, column);
    }
}

pub fn parse_result(result_ptr: *mut i32) -> Result<Program, String> {
    unsafe {
        match result_type(result_ptr) {
            0 => Ok(parse_program(result_ptr)),
            1 => Err(parse_error(result_ptr)),
            _ => panic!("Unexpected result type"),
        }
    }
}

pub fn parse_code(code: String) -> Result<Program, String> {
    let result;
    unsafe {
        initialize_haskell();

        let code_c = CString::new(code).expect("CString::new failed");
        let result_ptr = amethyst_parser(code_c.as_ptr());
        result = parse_result(result_ptr);
        free_result(result_ptr);

        exit_haskell();
    }
    result
}
