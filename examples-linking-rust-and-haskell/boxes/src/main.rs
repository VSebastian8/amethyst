use std::ffi::c_int;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn generateList() -> *mut MyList;
}

#[repr(C)]
struct Box {
    value: c_int,
}

#[repr(C)]
struct MyList {
    list_length: usize,
    list_elements: *const Box,
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
        let length = my_list.list_length;
        let elements = std::slice::from_raw_parts(my_list.list_elements, length);

        let rust_boxes: Vec<i32> = elements.iter().map(|b| b.value).collect();

        println!("Haskell list received: {:?}", rust_boxes);
        exit_haskell();
    }
}
