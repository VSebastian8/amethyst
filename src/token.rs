use std::fmt::Display;

use crate::info::Info;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // keywords
    Automaton,
    State,
    Initial,
    Accept,
    Reject,
    As,
    // punctuation
    LParanthesis,
    RParanthesis,
    LBracket,
    RBracket,
    Slash,
    Comma,
    Semicolon,
    Dot,
    Arrow,
    // literals
    Symbol(char),
    Ident(String),
}

impl Token {
    pub fn to_string(&self) -> String {
        match self {
            Token::Automaton => "keyword `automaton`",
            Token::State => "keyword `state`",
            Token::Initial => "keyword `initial`",
            Token::Accept => "keyword `accept`",
            Token::Reject => "keyword `reject`",
            Token::As => "keyword `as`",
            Token::LParanthesis => "`(`",
            Token::RParanthesis => "`)`",
            Token::LBracket => "`{`",
            Token::RBracket => "`}`",
            Token::Slash => "`/`",
            Token::Comma => "`,`",
            Token::Semicolon => "`;`",
            Token::Dot => "`.`",
            Token::Arrow => "`->`",
            Token::Symbol(ch) => return format!("symbol `{}`", ch),
            Token::Ident(x) => return format!("identifier `{}`", x),
        }
        .to_string()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub info: Info,
}
