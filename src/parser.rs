// Call Haskell Parser through FFI
// Construct the resulting syntax
use std::ffi::CString;
use std::os::raw::c_char;

use crate::syntax::*;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn parse(input: *const c_char) -> *mut i32;
    fn result_type(res: *mut i32) -> i32;
    fn return_error(res: *mut i32) -> *mut c_char;
}

fn parse_program(program: *mut i32) -> Program {
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
