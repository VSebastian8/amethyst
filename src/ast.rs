use std::rc::Rc;

use crate::info::ErrorInfo;
use crate::info::StringInfo;

#[derive(Debug, Clone, PartialEq)]
pub enum Move {
    L,
    R,
    N,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Transition {
    pub read: char,
    pub write: char,
    pub mov: Move,
    pub state: (StringInfo, Option<StringInfo>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateType {
    Accept,
    Reject,
    State(Option<StringInfo>, bool, Vec<Transition>), //  parent, initial, transitions
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub name: StringInfo,
    pub typ: StateType,
    pub desc: Rc<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Automaton {
    pub name: StringInfo,
    pub components: Vec<(StringInfo, StringInfo)>,
    pub states: Vec<State>,
    pub desc: Rc<str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub automata: Vec<Automaton>,
    pub errors: Vec<ErrorInfo>,
}
