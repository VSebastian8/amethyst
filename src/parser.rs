// Call Haskell Parser through FFI
// Construct the resulting syntax
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use crate::syntax::*;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    // Move function
    fn move_type(move_ptr: *mut i32) -> i32;
    // Transition functions
    fn transition_read_symbol(transition_ptr: *mut i32) -> c_char;
    fn transition_write_symbol(transition_ptr: *mut i32) -> c_char;
    fn transition_move_symbol(transition_ptr: *mut i32) -> *mut i32;
    fn transition_new_state(transition_ptr: *mut i32) -> *mut c_char;
    fn free_transition(transition_ptr: *mut i32);
    // State functions
    fn state_type(state: *mut i32) -> i32;
    fn state_name(state: *mut i32) -> *mut c_char;
    // Result functions
    fn result_type(res: *mut i32) -> i32;
    fn return_error(res: *mut i32) -> *mut c_char;
    fn parse(input: *const c_char) -> *mut i32;
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
        // Always free the pointer after processing the data
        free_transition(transition_ptr);

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
            2 => StateType::Accept(state_name),
            _ => panic!("Unexpected state type"),
        }
    }
}

fn parse_program(_program: *mut i32) -> Program {
    println!("Got back a Program");
    Program {
        automata: Box::new([]),
    }
}

fn parse_error(error: *mut i32) -> String {
    println!("Got back an Error");
    unsafe {
        let err_ptr = return_error(error);
        let c_err = CString::from_raw(err_ptr);
        c_err.to_str().expect("CString failed!").to_owned()
    }
}

#[allow(dead_code)]
pub fn call_parser() -> Result<Program, String> {
    let result;
    unsafe {
        initialize_haskell();
        let s = CString::new("hi there!").expect("CString::new failed");
        let res = parse(s.as_ptr());
        result = match result_type(res) {
            0 => Ok(parse_program(res)),
            1 => Err(parse_error(res)),
            _ => panic!("Unexpected result type from parser!"),
        };

        exit_haskell();
    }
    result
}
