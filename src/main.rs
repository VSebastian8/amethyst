use amethyst::parser::parse_code;

fn main() {
    println!("Amethyst - Turing Machine Programming Language");
    let res = parse_code("automata acceptor = override(R, 4, '0'); automata main(acceptor a1){initial state q0 {0 / 0, R -> q0; _ / B, N -> final;} accept state final; }".to_owned());
    println!("{:?}", res);
}
