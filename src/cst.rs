use std::rc::Rc;

use crate::info::Error;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Move {
    L,
    R,
    N,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
    // symbol w* / w* symbol w* , w* move w* -> w* (state|component.state) w* ;
    pub read: char,
    pub write: char,
    pub mov: Move,
    pub state: (Rc<str>, Option<Rc<str>>),
    pub w: Rc<[usize]>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransitionScope {
    Transition(Transition),
    ErrorTokens {
        error: Error,
        location: Option<usize>, // report error at specific token
        tokens: Vec<Token>,
    },
    LineComment(Rc<str>),
    BlockComment(Rc<str>),
    Whitespace,
    Newline,
}
