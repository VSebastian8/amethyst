#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Move {
    Left,
    Right,
    Neutral,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Transition {
    pub read_symbol: char,
    pub write_symbol: char,
    pub move_symbol: Move,
    pub new_state: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct State {
    pub initial: bool,
    pub transitions: Box<Vec<Transition>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StateType {
    Accept(String),
    Reject(String),
    State(String, State),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MacroType {
    Complement(String),
    Intersect(Box<Vec<String>>),
    Reunion(Box<Vec<String>>),
    Chain(Box<Vec<String>>),
    Repeat(String, u32),
    Move(Move, u32),
    Override(Move, u32, char),
    Place(String),
    Shift(Move, u32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Machine {
    pub components: Box<Vec<(String, String)>>,
    pub states: Box<Vec<StateType>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AutomatonType {
    Machine(String, Machine),
    Macro(String, MacroType),
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub automata: Box<Vec<AutomatonType>>,
}
