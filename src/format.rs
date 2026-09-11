use std::rc::Rc;

use crate::cst::*;
use crate::token::Token::*;
use crate::token::*;

struct Serializer(Vec<Token>);

fn move_symbol(mov: Move) -> Token {
    match mov {
        Move::L => Symbol('L'),
        Move::R => Symbol('R'),
        Move::N => Symbol('N'),
    }
}

impl Serializer {
    pub fn new() -> Self {
        Self(vec![])
    }

    fn wtoks(&mut self, w: u32) {
        self.0.extend(vec![Whitespace; w as usize]);
    }

    fn serialize_error(&mut self, et: ErrorTokens) {
        self.0.extend(et.tokens);
    }

    fn serialize_state_name(&mut self, state: (Rc<str>, Option<Rc<str>>), desc: Rc<str>) {
        if let Some(parent) = state.1 {
            self.0.push(Ident(parent, desc));
            self.0.push(Dot);
        }
        self.0.push(Ident(state.0, "".into()));
    }

    fn serialize_transition(&mut self, transition: Transition) {
        let Transition {
            read,
            write,
            mov,
            state,
            w,
        } = transition;
        self.0.push(Symbol(read));
        self.wtoks(w[0]);
        self.0.push(Slash);
        self.wtoks(w[1]);
        self.0.push(Symbol(write));
        self.wtoks(w[2]);
        self.0.push(Comma);
        self.wtoks(w[3]);
        self.0.push(move_symbol(mov));
        self.wtoks(w[4]);
        self.0.push(Arrow);
        self.wtoks(w[5]);
        self.serialize_state_name(state, "".into());
        self.wtoks(w[6]);
        self.0.push(Semicolon);
    }

    fn serialize_transitions(&mut self, scope: Vec<TransitionScope>) {
        self.0.push(LBracket);
        for t in scope {
            match t {
                TransitionScope::Whitespace => self.0.push(Whitespace),
                TransitionScope::Newline => self.0.push(Newline),
                TransitionScope::LineComment(x) => self.0.push(LineComment(x)),
                TransitionScope::BlockComment(x) => self.0.push(BlockComment(x)),
                TransitionScope::ErrorTokens(et) => self.serialize_error(et),
                TransitionScope::Transition(transition) => self.serialize_transition(transition),
            }
        }
        self.0.push(RBracket);
    }

    fn serialize_final_state(&mut self, state: FinalState) {
        let FinalState {
            accept,
            state,
            desc,
            w,
        } = state;
        self.0.push(if accept { Accept } else { Reject });
        self.wtoks(w[0]);
        self.0.push(State);
        self.wtoks(w[1]);
        self.0.push(Ident(state, desc));
        self.wtoks(w[2]);
        self.0.push(Semicolon);
    }

    fn serialize_transition_state(&mut self, state: TransitionState) {
        let TransitionState {
            initial,
            state,
            desc,
            w,
        } = state;
        if initial {
            self.0.push(Initial);
        }
        self.wtoks(w[0]);
        self.0.push(State);
        self.wtoks(w[1]);
        self.serialize_state_name(state, desc);
        self.wtoks(w[2]);
    }

    fn serialize_arrow_state(&mut self, state: ArrowState) {
        let ArrowState {
            initial,
            state,
            new_state,
            desc,
            w,
        } = state;
        if initial {
            self.0.push(Initial);
        }
        self.wtoks(w[0]);
        self.0.push(State);
        self.wtoks(w[1]);
        self.serialize_state_name(state, desc);
        self.wtoks(w[2]);
        self.0.push(Arrow);
        self.wtoks(w[3]);
        self.serialize_state_name(new_state, "".into());
        self.wtoks(w[4]);
        self.0.push(Semicolon);
    }

    fn serialize_states(&mut self, scope: Vec<StateScope>) {
        self.0.push(LBracket);
        for s in scope {
            match s {
                StateScope::Whitespace => self.0.push(Whitespace),
                StateScope::Newline => self.0.push(Newline),
                StateScope::LineComment(x) => self.0.push(LineComment(x)),
                StateScope::BlockComment(x) => self.0.push(BlockComment(x)),
                StateScope::ErrorTokens(et) => self.serialize_error(et),
                StateScope::FinalState(state) => self.serialize_final_state(state),
                StateScope::TransitionState(state) => self.serialize_transition_state(state),
                StateScope::ArrowState(state) => self.serialize_arrow_state(state),
                StateScope::Transitions(scope) => self.serialize_transitions(scope),
            }
        }
        self.0.push(RBracket);
    }

    fn serialize_component(&mut self, component: Component) {
        let Component {
            blueprint,
            alias,
            w,
        } = component;
        self.0.push(Ident(blueprint, "".into()));
        self.wtoks(w[0]);
        self.0.push(As);
        self.wtoks(w[1]);
        self.0.push(Ident(alias, "".into()));
    }

    fn serialize_components(&mut self, scope: Vec<ComponentScope>) {
        self.0.push(LParanthesis);
        for c in scope {
            match c {
                ComponentScope::Whitespace => self.0.push(Whitespace),
                ComponentScope::Newline => self.0.push(Newline),
                ComponentScope::Comma => self.0.push(Comma),
                ComponentScope::ErrorTokens(et) => self.serialize_error(et),
                ComponentScope::Component(component) => self.serialize_component(component),
            }
        }
        self.0.push(RParanthesis);
    }

    fn serialize_cst(&mut self, cst: Cst) {
        for a in cst {
            match a {
                AutomatonScope::Whitespace => self.0.push(Whitespace),
                AutomatonScope::Newline => self.0.push(Newline),
                AutomatonScope::LineComment(x) => self.0.push(LineComment(x)),
                AutomatonScope::BlockComment(x) => self.0.push(BlockComment(x)),
                AutomatonScope::ErrorTokens(et) => self.serialize_error(et),
                AutomatonScope::Automaton { name, desc, w } => {
                    self.0.push(Automaton);
                    self.wtoks(w);
                    self.0.push(Ident(name, desc));
                }
                AutomatonScope::Components(scope) => self.serialize_components(scope),
                AutomatonScope::States(scope) => self.serialize_states(scope),
            }
        }
    }
}

pub fn serialize(cst: Cst) -> Vec<Token> {
    let mut serializer = Serializer::new();
    serializer.serialize_cst(cst);
    serializer.0
}

pub fn stringify(tokens: Vec<Token>) -> String {
    tokens
        .iter()
        .map(|t| t.to_str())
        .collect::<Vec<Rc<str>>>()
        .join("")
}

fn format_transition(transition: Transition) -> Transition {
    Transition {
        w: [1, 1, 0, 1, 1, 1, 0],
        ..transition
    }
}

fn format_transitions(transitions: &Vec<TransitionScope>) -> Vec<TransitionScope> {
    let indented = transitions
        .iter()
        .filter(|t| **t != TransitionScope::Whitespace && **t != TransitionScope::Newline)
        .map(|t| match t {
            TransitionScope::Transition(transition) => {
                TransitionScope::Transition(format_transition(transition.clone()))
            }
            other => other.clone(),
        })
        .flat_map(|x| {
            vec![TransitionScope::Whitespace; 8]
                .into_iter()
                .chain([x, TransitionScope::Newline])
        });
    std::iter::once(TransitionScope::Newline)
        .chain(indented)
        .chain(vec![TransitionScope::Whitespace; 4].into_iter())
        .collect()
}

fn format_final_state(state: FinalState) -> FinalState {
    FinalState {
        w: [1, 1, 0],
        ..state
    }
}

fn format_arrow_state(state: ArrowState) -> ArrowState {
    ArrowState {
        w: [if state.initial { 1 } else { 0 }, 1, 1, 1, 0],
        ..state
    }
}

fn format_transition_state(state: TransitionState) -> TransitionState {
    TransitionState {
        w: [if state.initial { 1 } else { 0 }, 1, 1],
        ..state
    }
}

fn format_states(scope: &Vec<StateScope>) -> Vec<StateScope> {
    let mut states = vec![];
    let indent = vec![StateScope::Whitespace; 4];
    let mut state_dec = false;
    for st in scope {
        if *st == StateScope::Whitespace || *st == StateScope::Newline {
            continue;
        }
        if !state_dec || !matches!(st, StateScope::Transitions(_)) {
            states.push(StateScope::Newline);
            states.extend(indent.clone());
        }
        state_dec = false;
        states.push(match st {
            StateScope::FinalState(state) => {
                StateScope::FinalState(format_final_state(state.clone()))
            }
            StateScope::ArrowState(state) => {
                StateScope::ArrowState(format_arrow_state(state.clone()))
            }
            StateScope::TransitionState(state) => {
                state_dec = true;
                StateScope::TransitionState(format_transition_state(state.clone()))
            }
            StateScope::Transitions(scope) => StateScope::Transitions(format_transitions(scope)),
            x => x.clone(),
        })
    }
    states.push(StateScope::Newline);
    states
}

fn format_component(component: Component) -> Component {
    Component {
        w: [1, 1],
        ..component
    }
}

fn format_components(scope: &Vec<ComponentScope>) -> Vec<ComponentScope> {
    scope
        .into_iter()
        .filter(|c| **c != ComponentScope::Whitespace && **c != ComponentScope::Newline)
        .flat_map(|c| match c {
            ComponentScope::Comma => vec![ComponentScope::Comma, ComponentScope::Whitespace],
            ComponentScope::Component(component) => {
                vec![ComponentScope::Component(format_component(
                    component.clone(),
                ))]
            }
            x => vec![x.clone()],
        })
        .collect()
}

pub fn format(cst: &Cst) -> Cst {
    let mut automata = vec![];
    let mut first = true;
    let mut auto_dec = false;
    for a in cst {
        if a == &AutomatonScope::Whitespace || a == &AutomatonScope::Newline {
            continue;
        }
        if !auto_dec
            || !(matches!(a, AutomatonScope::Components(_))
                || matches!(a, AutomatonScope::States(_)))
        {
            auto_dec = false;
            if !first {
                automata.push(AutomatonScope::Newline);
            }
        }
        first = false;
        if auto_dec && matches!(a, AutomatonScope::States(_)) {
            auto_dec = false;
            automata.push(AutomatonScope::Whitespace);
        }
        automata.push(match a {
            AutomatonScope::Automaton { name, desc, .. } => {
                auto_dec = true;
                AutomatonScope::Automaton {
                    name: name.clone(),
                    desc: desc.clone(),
                    w: 1,
                }
            }
            AutomatonScope::Components(scope) => {
                AutomatonScope::Components(format_components(scope))
            }
            AutomatonScope::States(scope) => AutomatonScope::States(format_states(scope)),
            x => x.clone(),
        })
    }
    automata
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn test_reflective() {
        let code = " 
          automaton main() {
            // Okk
            initial state ok err {
                A / B, L -> c; other  nonsense
            } 
            accept state ok { nothing }
          }
        ";
        let tokens = Lexer::new(code).tokenize();
        let cst = Parser::new(tokens.clone()).parse();
        let serialized = serialize(cst);
        assert_eq!(tokens, serialized);
    }

    #[test]
    fn test_idempotent() {
        let code = " 
          automaton huh() {
            // Okk
            initial state ok err {
                {-   okk    -}
                X / hmmm ;
                A / B, R -> new.old;
            } 
          }
";
        let tokens = Lexer::new(code).tokenize();
        let code2 = stringify(tokens.clone());
        assert_eq!(code, code2);

        let cst = Parser::new(tokens).parse();
        let formatted = format(&cst);
        let formatted2 = format(&formatted);
        assert_eq!(formatted, formatted2);
    }

    #[test]
    fn test_formatting() {
        let code = " 
{- This does important work -}
automaton   main () {
    // Not really
// But it's not formatted
    initial   state  ok err {
        {-   okk    -}
        X  / no formatting in error   hmmm ;
        A /   B,  R  ->  new.old  ; 0/1,L->kay;
}}
";
        let tokens = Lexer::new(code).tokenize();
        let cst = Parser::new(tokens).parse();
        let formatted = format(&cst);
        let serialized = serialize(formatted);
        let code_formatted = stringify(serialized);
        assert_eq!(
            code_formatted,
            "{- This does important work -}
automaton main() {
    // Not really
    // But it's not formatted
    initial state ok 
    err 
    {
        {-   okk    -}
        X  / no formatting in error   hmmm ;
        A / B, R -> new.old;
        0 / 1, L -> kay;
    }
}"
        );
    }
}
