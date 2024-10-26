extern "C" {
    fn use_triple(val: i32) -> i32;
}

fn main() {
    let n = 8;
    let triple_n = unsafe { use_triple(n)};
    println!("{} * 3 = {}", n, triple_n);
}
