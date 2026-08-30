use crate::token::TokenInfo;
use std::{fmt::Display, rc::Rc};

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

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unknown {
        typ: Rc<str>,
        found: Rc<str>,
        info: Info,
    },
    NotTerminated {
        start: Rc<str>,
        end: Rc<str>,
        info: Info,
    },
    MalformedIdentifier {
        ident: Rc<str>,
        info: Info,
    },
    EOF {
        expected: Rc<str>,
    },
    Unexpected {
        expected: Rc<str>,
        token: TokenInfo,
    },
    Missing {
        expected: Rc<str>,
        info: Info,
    },
    Defined {
        typ: Rc<str>,
        name: Rc<str>,
        info: Info,
    },
    NotAllowed {
        reason: Rc<str>,
        info: Info,
    },
    Cycle {
        typ: Rc<str>,
        name: Rc<str>,
        info: Info,
    },
    Other {
        msg: Rc<str>,
    },
}

impl Error {
    pub fn message(&self) -> String {
        match self {
            Error::Unknown { typ, found, .. } => format!("Unknown {} {}", typ, found),
            Error::NotTerminated { start, end, .. } => {
                format!("Not terminated {}, maybe add {}", start, end)
            }
            Error::MalformedIdentifier { ident, .. } => {
                format!(
                    "Malformed identifier {}, allowed symbols a-z, 0-9 and _",
                    ident
                )
            }
            Error::Unexpected { token, expected } => {
                format!("Expected {}, found {}", expected, token.token)
            }
            Error::EOF { expected } => format!("Reached EndOfFile, expected {}", expected),
            Error::Missing { expected, .. } => format!("Missing {}", expected),
            Error::Defined { typ, name, .. } => format!("{} {} defined already", typ, name),
            Error::NotAllowed { reason, .. } => format!("{} is not allowed", reason),
            Error::Cycle { typ, name, .. } => format!("Found {} cycle in {}", typ, name), // TODO: better cycle message (trace)
            Error::Other { msg } => msg.to_string(),
        }
    }
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

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}
