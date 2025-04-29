#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Move {
    Left,
    Right,
    Neutral,
}

#[derive(Debug, PartialEq)]
pub struct Transition {
    pub read_symbol: char,
    pub write_symbol: char,
    pub move_symbol: Move,
    pub new_state: String,
}

#[derive(Debug, PartialEq)]
pub struct State {
    pub initial: bool,
    pub transitions: Box<Vec<Transition>>,
}

#[derive(Debug, PartialEq)]
pub enum StateType {
    Accept(String),
    Reject(String),
    State(String, State),
}

#[derive(Debug, PartialEq)]
pub enum MacroType {
    Complement(String),
    Intersect(Box<Vec<String>>),
    Reunion(Box<Vec<String>>),
    Chain(Box<Vec<String>>),
    Repeat(i32, String),
    Move(Move, i32),
    Override(Move, i32, char),
    Place(String),
    Shift(Move, i32),
}

#[derive(Debug, PartialEq)]
pub struct Machine {
    pub components: Box<Vec<(String, String)>>,
    pub states: Box<Vec<StateType>>,
}

#[derive(Debug, PartialEq)]
pub enum AutomataType {
    Machine(String, Machine),
    Macro(String, MacroType),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub automata: Box<Vec<AutomataType>>,
}
