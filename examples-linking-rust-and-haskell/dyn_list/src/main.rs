use std::ffi::c_int;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn generateList() -> *mut MyList;
}

#[repr(C)]
struct MyList {
    length: usize,
    elements: *const c_int,
}

fn main() {
    unsafe {
        initialize_haskell();
        let ptr = generateList();
        if ptr.is_null() {
            eprintln!("Failed to get list.");
            return;
        }

        let my_list = &*ptr;
        let elements = std::slice::from_raw_parts(my_list.elements, my_list.length);

        println!("Haskell list received: {:?}", elements);

        exit_haskell();
    }
}
