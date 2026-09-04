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
    LineComment(Rc<str>),
    BlockComment(Rc<str>),
    Unknown(char),
}

impl Token {
    pub fn debug(&self) -> Rc<str> {
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
            Token::Symbol(ch) => return format!("symbol `{}`", ch).into(),
            Token::Ident(x, _) => return format!("identifier `{}`", x).into(),
            Token::Whitespace => "` `",
            Token::Newline => "`\\n`",
            Token::LineComment(x) => return format!("line comment `{}`", x).into(),
            Token::BlockComment(x) => return format!("block comment `{}`", x).into(),
            Token::Unknown(x) => return format!("character `{}`", x).into(),
        }
        .into()
    }

    pub fn to_str(&self) -> Rc<str> {
        match self {
            Token::Automaton => "automaton",
            Token::State => "state",
            Token::Initial => "initial",
            Token::Accept => "accept",
            Token::Reject => "reject",
            Token::As => "as",
            Token::LParanthesis => "(",
            Token::RParanthesis => ")",
            Token::LBracket => "{",
            Token::RBracket => "}",
            Token::Slash => "/",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::Dot => ".",
            Token::Arrow => "->",
            Token::Symbol(ch) => return format!("{}", ch).into(),
            Token::Ident(x, _) => x,
            Token::Whitespace => " ",
            Token::Newline => "\n",
            Token::LineComment(x) => return format!("//{}", x).into(),
            Token::BlockComment(x) => return format!("{{-{}-}}", x).into(),
            Token::Unknown(x) => return format!("{}", x).into(),
        }
        .into()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg: Rc<str> = self.to_str();
        write!(f, "{}", msg)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub info: Info,
}
