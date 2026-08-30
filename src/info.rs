use crate::token::TokenInfo;
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
    Unknown {
        typ: Rc<str>,
        found: Rc<str>,
        info: Info,
    },
    #[error("Not terminated {start}, maybe add {end}")]
    NotTerminated {
        start: Rc<str>,
        end: Rc<str>,
        info: Info,
    },
    #[error("Malformed identifier {ident}, allowed symbols a-z, 0-9 and _")]
    MalformedIdentifier { ident: Rc<str>, info: Info },
    #[error("Reached End Of File, expected {expected}")]
    EOF { expected: Rc<str> },
    #[error("Expected {expected}, found {}", token.token)]
    Unexpected { expected: Rc<str>, token: TokenInfo },
    #[error("Missin {expected}")]
    Missing { expected: Rc<str>, info: Info },
    #[error("{typ} {name} defined already")]
    Defined {
        typ: Rc<str>,
        name: Rc<str>,
        info: Info,
    },
    #[error("{reason} is not allowed")]
    NotAllowed { reason: Rc<str>, info: Info },
    #[error("Found {typ} cycle in {name}")]
    Cycle {
        typ: Rc<str>,
        name: Rc<str>,
        info: Info,
    },
    #[error("{msg}")]
    Other { msg: Rc<str> },
}

impl Error {
    pub fn info(&self) -> Option<&Info> {
        match self {
            Error::Unknown { info, .. } => Some(info),
            Error::NotTerminated { info, .. } => Some(info),
            Error::MalformedIdentifier { info, .. } => Some(info),
            Error::Unexpected { token, .. } => Some(&token.info),
            Error::Missing { info, .. } => Some(info),
            Error::Defined { info, .. } => Some(info),
            Error::NotAllowed { info, .. } => Some(info),
            Error::Cycle { info, .. } => Some(info),
            Error::EOF { .. } => None,
            Error::Other { .. } => None,
        }
    }
    pub fn print_context(&self) {
        let Info { line, from, to } = match self.info() {
            Some(info) => info,
            _ => return,
        };
        println!("At line {}, columns {} - {}:", line + 1, from + 1, to + 1);
    }
}
