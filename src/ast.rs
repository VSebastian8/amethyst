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
    pub state: (String, Option<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum State {
    Accept(String),
    Reject(String),
    State(String, Option<String>, bool, Vec<Transition>), // name, parent, initial, transitions
}

#[derive(Debug, Clone, PartialEq)]
pub struct Automaton {
    pub name: String,
    pub components: Vec<(String, String)>,
    pub states: Vec<State>,
}
