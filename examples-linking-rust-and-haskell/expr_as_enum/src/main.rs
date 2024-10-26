use std::ptr;

extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    fn createExpr(x: i32, y: i32) -> *mut i32;
    fn createComplexExpr() -> *mut i32;
    fn evaluateExpr(expr: *mut i32) -> i32;
    fn expressionType(expr: *mut i32) -> i32;
    fn getNumber(expr: *mut i32) -> i32;
    fn getFirstAdd(expr: *mut i32) -> *mut i32;
    fn getSecondAdd(expr: *mut i32) -> *mut i32;
}

#[derive(Debug)]
enum Expr {
    Zero,
    Number(i32),
    Add(Box<Expr>, Box<Expr>),
}

fn make_expression(expr: *mut i32) -> Expr {
    unsafe {
        match expressionType(expr) {
            0 => Expr::Zero,
            1 => Expr::Number(getNumber(expr)),
            _ => Expr::Add(
                Box::new(make_expression(getFirstAdd(expr))),
                Box::new(make_expression(getSecondAdd(expr))),
            ),
        }
    }
}

fn eval_expression(expr: Expr) -> i32 {
    match expr {
        Expr::Zero => 0,
        Expr::Number(n) => n,
        Expr::Add(e1, e2) => eval_expression(*e1) + eval_expression(*e2),
    }
}

fn main() {
    let x = 5;
    let y = 10;

    unsafe {
        initialize_haskell();
        let expr1 = createExpr(x, y);
        if expr1 != ptr::null_mut() {
            let result = evaluateExpr(expr1);
            println!("Expression1: Add(Number({}), Number({}))", x, y);
            println!("Evaluation from Haskell of expr1: {}", result);
        } else {
            println!("Failed to create expression.");
        }
        println!("");

        let expr2 = createComplexExpr();
        let rust_expr = make_expression(expr2);
        println!("Expression2: {:?}", rust_expr);
        println!(
            "Evaluation from Rust of expr2: {}",
            eval_expression(rust_expr)
        );

        exit_haskell();
    }
}
