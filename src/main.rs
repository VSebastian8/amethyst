mod parser;
mod syntax;
#[allow(unused_imports)]
use crate::parser::{call_parser, test_transition_parser};

fn main() {
    // let r = call_parser();
    // match r {
    //     Ok(p) => println!("{:?}", p),
    //     Err(e) => println!("{}", e),
    // }
    let t = test_transition_parser();
    println!("{:?}", t);
}
