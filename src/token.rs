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

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub info: Info,
}
