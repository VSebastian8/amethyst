use std::fmt::Display;

use crate::token::TokenInfo;

#[derive(Debug, Clone, PartialEq)]
pub struct Info {
    pub line: u32,
    pub from: u32,
    pub to: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unknown(char, Info),
    NotTerminated(String, String, Info),
    MalformedIdentifier(String, Info),
    EOF(String),
    Unexpected(TokenInfo, String),
    Missing(String, Info),
    Other(String),
}

impl Error {
    pub fn message(&self) -> String {
        match self {
            Error::Unknown(ch, _) => format!("Unknown character {}", ch),
            Error::NotTerminated(start, end, _) => {
                format!("Not terminated {}, maybe add {}", start, end)
            }
            Error::MalformedIdentifier(x, _) => {
                format!("Malformed identifier {}, allowed symbols a-z, 0-9 and _", x)
            }
            Error::Unexpected(token, str) => format!("Expected {}, found {}", str, token.token),
            Error::EOF(msg) => format!("Reached EndOfFile, expected {}", msg),
            Error::Missing(expected, _) => format!("Missing {}", expected),
            Error::Other(msg) => msg.to_string(),
        }
    }
    pub fn info(&self) -> Option<&Info> {
        match self {
            Error::Unknown(_, info) => Some(info),
            Error::NotTerminated(_, _, info) => Some(info),
            Error::MalformedIdentifier(_, info) => Some(info),
            Error::Unexpected(tok, _) => Some(&tok.info),
            Error::Missing(_, info) => Some(info),
            _ => None,
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
