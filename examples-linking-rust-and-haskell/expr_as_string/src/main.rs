use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    fn call_haskell_function(a: i32, b: i32) -> *mut c_char;
}

fn main() {
    let a = 10;
    let b = 20;
    unsafe {
        let output = call_haskell_function(a, b);
        let c_str_output = CStr::from_ptr(output);
        println!("Created Expr: {}", c_str_output.to_str().unwrap());
    }
}
