extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn createShape(n: i32) -> *mut i32;
    fn isSquare(shape: *mut i32) -> i32;
    fn getSide(shape: *mut i32) -> i32;
    fn getWidth(shape: *mut i32) -> i32;
    fn getLength(shape: *mut i32) -> i32;
}

enum Shape {
    Square(i32),
    Rectangle(i32, i32),
}

fn print_shape(shape: Shape) {
    match shape {
        Shape::Square(s) => println!("Square with side: {}", s),
        Shape::Rectangle(w, l) => println!("Rectangle with width: {} and length: {}", w, l),
    }
}

fn main() {
    let n: i32 = 15;
    let m = 8;
    unsafe {
        initialize_haskell();
        let shape1 = createShape(n);
        if isSquare(shape1) == 1 {
            println!("Square with side: {}", getSide(shape1));
        } else {
            println!(
                "Rectangle with width: {} and length: {}",
                getWidth(shape1),
                getLength(shape1)
            );
        }

        let shape2 = createShape(m);
        let rust_shape: Shape = if isSquare(shape2) == 1 {
            Shape::Square(getSide(shape2))
        } else {
            Shape::Rectangle(getWidth(shape2), getLength(shape2))
        };
        print_shape(rust_shape);
        exit_haskell();
    }
}
