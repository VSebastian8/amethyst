#[derive(Debug)]
pub enum Move {
    Left,
    Right,
    Neutral,
}

#[derive(Debug)]
pub struct Transition {
    pub readSymbol: char,
    pub writeSymbol: char,
    pub moveSymbol: Move,
    pub newState: String,
}

#[derive(Debug)]
pub struct State {
    pub transitions: Box<[Transition]>,
    pub initial: bool,
}

#[derive(Debug)]
pub enum StateType {
    Accept(String),
    Reject(String),
    State(String, State),
}

#[derive(Debug)]
pub enum MacroType {
    Complement(String),
    Intersect(Box<[String]>),
    Reunion(Box<[String]>),
    Chain(Box<[String]>),
    Repeat(i32, String),
    Move(Move, i32),
    Override(Move, i32, char),
    Place(String),
    Shift(Move, i32),
}

#[derive(Debug)]
pub struct Machine {
    pub components: Box<[(String, String)]>,
    pub states: Box<[String]>,
}

#[derive(Debug)]
pub enum AutomataType {
    Machine(String, Machine),
    Macro(String, MacroType),
}

#[derive(Debug)]
pub struct Program {
    pub automata: Box<[AutomataType]>,
}
