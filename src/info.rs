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
    Other(String),
}

impl Error {
    pub fn print_context(&self) {
        let Info { line, from, to } = match self {
            Error::Unknown(_, info) => info,
            Error::NotTerminated(_, _, info) => info,
            Error::MalformedIdentifier(_, info) => info,
            Error::Unexpected(tok, _) => &tok.info,
            _ => return,
        };
        println!("At line {}, columns {} - {}:", line, from, to);
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Unknown(ch, _) => write!(f, "Unknown character {}", ch),
            Error::NotTerminated(start, end, _) => {
                write!(f, "Not terminated {}, maybe add {}", start, end)
            }

            Error::MalformedIdentifier(x, _) => write!(
                f,
                "Malformed identifier {}, allowed symbols a-z, 0-9 and _",
                x
            ),
            Error::Unexpected(token, str) => write!(f, "Expected {}, found {}", str, token.token),
            Error::EOF(msg) => write!(f, "Reached EndOfFile, expected {}", msg),
            Error::Other(msg) => write!(f, "{}", msg),
        }
    }
}
