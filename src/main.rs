use std::ffi::CString;
use std::os::raw::c_char;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn parse(input: *const c_char) -> *mut i32;
    fn result_type(res: *mut i32) -> i32;
    fn return_error(res: *mut i32) -> *mut c_char;
}

fn main() {
    let mut err: String = "".to_owned();
    unsafe {
        initialize_haskell();
        let s = CString::new("hi there!").expect("CString::new failed");
        let res = parse(s.as_ptr());
        match result_type(res) {
            0 => println!("Got back a Program"),
            1 => {
                println!("Got back an Error");
                let err_ptr = return_error(res);
                let c_err = CString::from_raw(err_ptr);
                err = c_err.to_str().expect("CString failed!").to_owned();
            }
            _ => panic!("Unexpected result type from parser!"),
        }

        exit_haskell();
    }
    println!("{}", err);
}
