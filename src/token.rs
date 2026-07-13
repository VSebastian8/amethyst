#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // keywords
    Automaton,
    State,
    Initial,
    Accept,
    Reject,
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
