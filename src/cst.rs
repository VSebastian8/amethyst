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
    pub w: [u32; 7],
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

#[derive(Debug, Clone, PartialEq)]
pub struct FinalState {
    // (accept|reject) w* state w* (state) w*;
    pub accept: bool,
    pub state: Rc<str>,
    pub desc: Rc<str>,
    pub w: [u32; 3],
}

#[derive(Debug, Clone, PartialEq)]
pub struct TransitionState {
    // (initial?) w* state w* (state|component.state) w*
    pub initial: bool,
    pub state: (Rc<str>, Option<Rc<str>>),
    pub desc: Rc<str>,
    pub w: [u32; 3],
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrowState {
    // (initial?) w* state w* (state|component.state) w* -> w* (state|component.state) w* ;
    pub initial: bool,
    pub state: (Rc<str>, Option<Rc<str>>),
    pub new_state: (Rc<str>, Option<Rc<str>>),
    pub desc: Rc<str>,
    pub w: [u32; 5],
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateScope {
    FinalState(FinalState),
    TransitionState(TransitionState),
    ArrowState(ArrowState),
    Transitions(Vec<TransitionScope>),
    ErrorTokens {
        error: Error,
        location: Option<usize>,
        tokens: Vec<Token>,
    },
    LineComment(Rc<str>),
    BlockComment(Rc<str>),
    Whitespace,
    Newline,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Component {
    // (ident) w* as w* (ident)
    pub blueprint: Rc<str>,
    pub alias: Rc<str>,
    pub w: [u32; 2],
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComponentScope {
    Component(Component),
    Comma,
    ErrorTokens {
        error: Error,
        location: Option<usize>,
        tokens: Vec<Token>,
    },
    Whitespace,
    Newline,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AutomatonScope {
    Automaton {
        name: Rc<str>,
        desc: Rc<str>,
        w: u32,
    },
    Components(Vec<ComponentScope>),
    States(Vec<StateScope>),
    ErrorTokens {
        error: Error,
        location: Option<usize>,
        tokens: Vec<Token>,
    },
    LineComment(Rc<str>),
    BlockComment(Rc<str>),
    Whitespace,
    Newline,
}

pub type Cst = Vec<AutomatonScope>;
