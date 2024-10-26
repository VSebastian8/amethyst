use std::ffi::{CStr, CString};
use std::os::raw::c_char;

extern "C" {
    fn call_haskell_function(input: *const c_char) -> *mut c_char;
}

fn main() {
    let input = CString::new("Hello, Haskell!").expect("CString::new failed");
    unsafe {
        let output = call_haskell_function(input.as_ptr());
        let c_str_output = CStr::from_ptr(output);
        println!("Reversed string: {}", c_str_output.to_str().unwrap());
    }
}
