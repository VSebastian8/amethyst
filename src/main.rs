mod parser;
mod syntax;

use crate::parser::call_parser;

fn main() {
    let r = call_parser();
    match r {
        Ok(p) => println!("{:?}", p),
        Err(e) => println!("{}", e),
    }
}
