use std::{fmt::Display, rc::Rc};

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
    Ident(Rc<str>, Rc<str>),
    // CST
    Whitespace,
    Newline,
    Comment(Rc<str>),
    Unknown(char),
}

impl From<Token> for Rc<str> {
    fn from(token: Token) -> Rc<str> {
        match token {
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
            Token::Symbol(ch) => return format!("symbol `{}`", ch).into(),
            Token::Ident(x, _) => return format!("identifier `{}`", x).into(),
            Token::Whitespace => "` `",
            Token::Newline => "`\\n`",
            Token::Comment(x) => return format!("comment `{}`", x).into(),
            Token::Unknown(x) => return format!("character `{}`", x).into(),
        }
        .into()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: avoid clone
        let msg: Rc<str> = self.clone().into();
        write!(f, "{}", msg)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub info: Info,
}
