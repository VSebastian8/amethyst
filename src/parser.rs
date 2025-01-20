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
    fn test_transition(n: i32) -> *mut i32;
    fn free_transition(transition_ptr: *mut i32);
    // Result functions
    fn result_type(res: *mut i32) -> i32;
    fn return_error(res: *mut i32) -> *mut c_char;
    fn parse(input: *const c_char) -> *mut i32;
}

fn parse_transition(transition: *mut i32) -> Transition {
    unsafe {
        let read_symbol = transition_read_symbol(transition) as u8 as char;
        let write_symbol = transition_write_symbol(transition) as u8 as char;
        // Get the move as a pointer and then find which move it is
        let move_ptr = transition_move_symbol(transition);
        let move_symbol = match move_type(move_ptr) {
            0 => Move::Left,
            1 => Move::Right,
            2 => Move::Neutral,
            _ => panic!("Unexpected move type"),
        };
        // Convert from CString to Rust String
        let new_state = CStr::from_ptr(transition_new_state(transition))
            .to_str()
            .expect("Error converting CString to String")
            .to_owned();
        // Always free the pointer after processing the data
        free_transition(transition);

        Transition {
            read_symbol,
            write_symbol,
            move_symbol,
            new_state,
        }
    }
}

pub fn test_transition_parser() -> Transition {
    let transition;
    unsafe {
        initialize_haskell();
        let transition_ptr = test_transition(1);
        transition = parse_transition(transition_ptr);
        exit_haskell();
    }
    transition
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
