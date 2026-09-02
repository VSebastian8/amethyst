use crate::token::Token;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Info {
    pub line: u32,
    pub from: u32,
    pub to: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringInfo {
    pub name: Rc<str>,
    pub info: Info,
}

impl StringInfo {
    // Used for terser tests
    pub fn from(name: &str) -> Self {
        StringInfo {
            name: name.into(),
            info: Info::default(),
        }
    }
}

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Clone)]
pub enum Error {
    #[error("Unknown {typ} {found}")]
    Unknown { typ: Rc<str>, found: Rc<str> },
    #[error("Not terminated {start}, maybe add {end}")]
    NotTerminated { start: Rc<str>, end: Rc<str> },
    #[error("Malformed identifier {ident}, allowed symbols a-z, 0-9 and _")]
    MalformedIdentifier { ident: Rc<str> },
    #[error("Reached End Of File, expected {expected}")]
    EOF { expected: Rc<str> },
    #[error("Expected {expected}, found {}", token)]
    Unexpected { expected: Rc<str>, token: Token },
    #[error("Missin {expected}")]
    Missing { expected: Rc<str> },
    #[error("{typ} {name} defined already")]
    Defined { typ: Rc<str>, name: Rc<str> },
    #[error("{reason} is not allowed")]
    NotAllowed { reason: Rc<str> },
    #[error("Found {typ} cycle in {name}")]
    Cycle { typ: Rc<str>, name: Rc<str> },
    #[error("{msg}")]
    Other { msg: Rc<str> },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ErrorInfo {
    pub error: Error,
    pub info: Option<Info>,
}

impl ErrorInfo {
    pub fn print_context(&self) {
        match self.info {
            None => {}
            Some(Info { line, from, to }) => {
                println!("At line {}, columns {} - {}:", line + 1, from + 1, to + 1)
            }
        };
    }
}
