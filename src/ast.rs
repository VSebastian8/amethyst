use crate::info::Error;
use crate::info::Info;

#[derive(Debug, Clone, PartialEq)]
pub enum Move {
    L,
    R,
    N,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringInfo {
    pub name: String,
    pub info: Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
    pub read: char,
    pub write: char,
    pub mov: Move,
    pub state: (String, Option<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateType {
    Accept,
    Reject,
    State(Option<String>, bool, Vec<Transition>), // name, parent, initial, transitions
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub name: String,
    pub typ: StateType,
    pub desc: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Automaton {
    pub name: String,
    pub components: Vec<(String, String)>,
    pub states: Vec<State>,
    pub desc: String,
}

pub struct Ast {
    pub automata: Vec<Automaton>,
    pub errors: Vec<Error>,
}
