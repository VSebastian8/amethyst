// Module handling file loading and code parsing
// will also handle gem(import) resolution

use crate::ast::Ast;
use crate::desugar::Desugarer;
use crate::info::{Error, ErrorInfo};
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fs;

pub fn parse_ast(code: &str) -> Ast {
    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize();

    let parser = Parser::new(tokens);
    let cst = parser.parse();
    let desugarer = Desugarer::new();
    desugarer.desugar(cst)
}

pub fn load_ast(filename: &str) -> Result<Ast, ErrorInfo> {
    fs::read_to_string(filename)
        .map(|code| parse_ast(&code))
        .map_err(|err| ErrorInfo {
            error: Error::Other {
                msg: err.to_string().into(),
            },
            info: None,
        })
}
