// Module handling file loading and code parsing
// will also handle gem(import) resolution

use crate::ast::Ast;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fs;

pub fn parse_ast(code: &str) -> Ast {
    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize();

    let parser = Parser::new(tokens);
    let Ast { automata, errors } = parser.parse();
    Ast {
        automata,
        errors: lexer.errors.into_iter().chain(errors).collect(),
    }
}

pub fn load_ast(filename: &str) -> Result<Ast, std::io::Error> {
    fs::read_to_string(filename).map(|code| parse_ast(&code))
}
