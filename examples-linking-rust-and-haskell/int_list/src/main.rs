extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn generateList() -> *mut i32;
}

fn main() {
    unsafe {
        initialize_haskell();
        let ptr = generateList();
        if ptr.is_null() {
            eprintln!("Failed to get list.");
            return;
        }

        // Assume length of the list for demonstration
        let length = 5;
        let list = std::slice::from_raw_parts(ptr, length);

        println!("Haskell list received: {:?}", list);

        // Process list
        let sum: i32 = list.iter().map(|&x| x as i32).sum();
        println!("Sum of list: {}", sum);
        exit_haskell();
    }
}
