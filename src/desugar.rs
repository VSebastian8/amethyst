use std::rc::Rc;

use crate::ast;
use crate::cst;
use crate::info::*;
use crate::token::Token;

pub struct Desugarer {
    pub errors: Vec<ErrorInfo>,
    pub line: u32,
    pub col: u32,
}

impl Desugarer {
    pub fn new() -> Self {
        Self {
            errors: vec![],
            line: 0,
            col: 0,
        }
    }

    fn desugar_whitespace(&mut self) {
        self.col += 1;
    }

    fn desugar_newline(&mut self) {
        self.line += 1;
        self.col = 0;
    }

    fn desugar_comment(&mut self, comment: Rc<str>) {
        for ch in comment.chars() {
            if ch == '\n' {
                self.desugar_newline();
            } else {
                self.desugar_whitespace();
            }
        }
        self.col += 2; // -}
    }

    fn desugar_token(&mut self, token: &Token) {
        self.col += match token {
            Token::Newline | Token::LineComment(_) => {
                self.desugar_newline();
                0
            }
            Token::BlockComment(comment) => {
                self.desugar_comment(comment.clone());
                0
            }
            Token::Whitespace
            | Token::LParanthesis
            | Token::RParanthesis
            | Token::LBracket
            | Token::RBracket
            | Token::Slash
            | Token::Comma
            | Token::Dot
            | Token::Semicolon
            | Token::Symbol(_)
            | Token::Unknown(_) => 1,
            Token::As | Token::Arrow => 2,
            Token::State | Token::Accept | Token::Reject => 6,
            Token::Initial => 7,
            Token::Automaton => 9,
            Token::Ident(name, _) => name.len(),
        } as u32
    }

    fn desugar_tokens(&mut self, tokens: Vec<Token>, location: Option<usize>) -> Option<Info> {
        match location {
            None => {
                for token in tokens {
                    self.desugar_token(&token);
                }
                None
            }
            Some(loc) => {
                let (before, after) = tokens.as_slice().split_at(std::cmp::min(loc, tokens.len()));
                for token in before {
                    self.desugar_token(token);
                }
                let line = self.line;
                let from = self.col;
                if !after.is_empty() {
                    self.desugar_token(&after[0]);
                }
                let to = self.col;
                if after.len() > 1 {
                    for token in &after[1..] {
                        self.desugar_token(token);
                    }
                }
                Some(Info { line, from, to })
            }
        }
    }

    fn desugar_error(&mut self, error_tokens: cst::ErrorTokens) {
        let cst::ErrorTokens {
            error,
            location,
            tokens,
        } = error_tokens;
        let info = self.desugar_tokens(tokens, location);
        self.errors.push(ErrorInfo { error, info });
    }

    fn desugar_move(&self, mov: cst::Move) -> ast::Move {
        match mov {
            cst::Move::L => ast::Move::L,
            cst::Move::R => ast::Move::R,
            cst::Move::N => ast::Move::N,
        }
    }

    fn state_info(&self, state: &(Rc<str>, Option<Rc<str>>)) -> [u32; 4] {
        let parent_from: u32 = self.col;
        let parent_to = parent_from + state.1.as_ref().map_or(0, |parent| parent.len() as u32);
        let state_from = parent_to + state.1.as_ref().map_or(0, |_| 1);
        let state_to = state_from + state.0.len() as u32;
        [parent_from, parent_to, state_from, state_to]
    }

    // parent.state | state -> (state, Option(parent))
    fn desugar_state_name(
        &mut self,
        state: (Rc<str>, Option<Rc<str>>),
    ) -> (StringInfo, Option<StringInfo>) {
        let state_info = self.state_info(&state);
        self.col = state_info[3];
        (
            StringInfo {
                name: state.0,
                info: Info {
                    line: self.line,
                    from: state_info[2],
                    to: state_info[3],
                },
            },
            state.1.map(|parent| StringInfo {
                name: parent,
                info: Info {
                    line: self.line,
                    from: state_info[0],
                    to: state_info[1],
                },
            }),
        )
    }

    // Construct AST Transition and skip the relevant columns
    fn desugar_transition(&mut self, transition: cst::Transition) -> ast::Transition {
        // 2 sym + "/,->"
        self.col += 7 + transition.w[0..6].iter().sum::<u32>();
        let state = self.desugar_state_name(transition.state);
        // Update column and return
        self.col += transition.w[6] + 1;
        ast::Transition {
            read: transition.read,
            write: transition.write,
            mov: self.desugar_move(transition.mov),
            state,
        }
    }

    fn desugar_transitions(&mut self, scope: Vec<cst::TransitionScope>) -> Vec<ast::Transition> {
        let mut transitions = vec![];
        self.col += 1; // {
        for t in scope {
            match t {
                cst::TransitionScope::Transition(transition) => {
                    transitions.push(self.desugar_transition(transition))
                }
                cst::TransitionScope::Whitespace => self.desugar_whitespace(),
                cst::TransitionScope::Newline | cst::TransitionScope::LineComment(_) => {
                    self.desugar_newline()
                }
                cst::TransitionScope::BlockComment(comment) => self.desugar_comment(comment),
                cst::TransitionScope::ErrorTokens(err) => self.desugar_error(err),
            }
        }
        self.col += 1; // }
        transitions
    }

    fn desugar_final_state(&mut self, state: cst::FinalState) -> ast::State {
        let cst::FinalState {
            accept,
            state,
            desc,
            w,
        } = state;
        // accept w* state w*
        let state_from = self.col + 6 + w[0] + 5 + w[1];
        let state_to = state_from + state.len() as u32;
        // w* ;
        self.col = state_to + w[2] + 1;
        ast::State {
            name: StringInfo {
                name: state.clone(),
                info: Info {
                    line: self.line,
                    from: state_from,
                    to: state_to,
                },
            },
            typ: if accept {
                ast::StateType::Accept
            } else {
                ast::StateType::Reject
            },
            desc,
        }
    }

    fn desugar_arrow_state(&mut self, state: cst::ArrowState) -> ast::State {
        let cst::ArrowState {
            initial,
            state,
            new_state,
            desc,
            w,
        } = state;
        self.col += if initial { 7 } else { 0 } + w[0] + 5 + w[1];
        let (state_name, parent_name) = self.desugar_state_name(state);
        self.col += w[2] + 2 + w[3];
        let new_state = self.desugar_state_name(new_state);
        self.col += w[4] + 1;
        ast::State {
            name: state_name,
            typ: ast::StateType::State(
                parent_name,
                initial,
                vec![ast::Transition {
                    read: '_',
                    write: '_',
                    mov: ast::Move::N,
                    state: new_state,
                }],
            ),
            desc,
        }
    }

    fn desugar_transition_state(&mut self, state: cst::TransitionState) -> ast::State {
        let cst::TransitionState {
            initial,
            state,
            desc,
            w,
        } = state;
        self.col += if initial { 7 } else { 0 } + w[0] + 5 + w[1];
        let (state_name, parent_name) = self.desugar_state_name(state);
        self.col += w[2];
        ast::State {
            name: state_name,
            typ: ast::StateType::State(parent_name, initial, vec![]),
            desc,
        }
    }

    fn clear_last_dec(
        &mut self,
        last_dec: &mut Option<(ast::State, Info)>,
        states: &mut Vec<ast::State>,
    ) {
        if let Some((state, info)) = last_dec.take() {
            states.push(state);
            self.errors.push(ErrorInfo {
                error: Error::Missing {
                    expected: "state transitions".into(),
                },
                info: Some(info),
            })
        }
    }

    fn desugar_states(&mut self, scope: Vec<cst::StateScope>) -> Vec<ast::State> {
        let mut states = vec![];
        let mut last_dec: Option<(ast::State, Info)> = None; // state without transitions
        self.col += 1; // {
        for st in scope {
            match st {
                cst::StateScope::Whitespace => self.desugar_whitespace(),
                cst::StateScope::Newline | cst::StateScope::LineComment(_) => {
                    self.desugar_newline()
                }
                cst::StateScope::BlockComment(comment) => self.desugar_comment(comment),
                cst::StateScope::ErrorTokens(err) => self.desugar_error(err),
                cst::StateScope::FinalState(state) => {
                    self.clear_last_dec(&mut last_dec, &mut states);
                    states.push(self.desugar_final_state(state));
                }
                cst::StateScope::ArrowState(state) => {
                    self.clear_last_dec(&mut last_dec, &mut states);
                    states.push(self.desugar_arrow_state(state));
                }
                cst::StateScope::TransitionState(state) => {
                    self.clear_last_dec(&mut last_dec, &mut states);
                    let from = self.col;
                    last_dec = Some((
                        self.desugar_transition_state(state),
                        Info {
                            line: self.line,
                            from,
                            to: self.col,
                        },
                    ));
                }
                cst::StateScope::Transitions(transitions) => {
                    if let Some((mut state, _)) = last_dec.take() {
                        match state.typ {
                            ast::StateType::State(_, _, ref mut trs) => {
                                *trs = self.desugar_transitions(transitions);
                            }
                            _ => {}
                        }
                        states.push(state);
                    } else {
                        self.errors.push(ErrorInfo {
                            error: Error::Missing {
                                expected: "state declaration".into(),
                            },
                            info: Some(Info {
                                line: self.line,
                                from: self.col,
                                to: self.col + 1,
                            }),
                        });
                        self.desugar_transitions(transitions);
                    }
                }
            }
        }
        self.col += 1; // }
        states
    }

    fn desugar_component(&mut self, component: cst::Component) -> (StringInfo, StringInfo) {
        let cst::Component {
            blueprint,
            alias,
            w,
        } = component;
        let blueprint_from = self.col;
        let blueprint_to = blueprint_from + blueprint.len() as u32;
        self.col = blueprint_to + w[0] + 2 + w[1];
        let alias_from = self.col;
        let alias_to = alias_from + alias.len() as u32;
        self.col = alias_to;
        (
            StringInfo {
                name: blueprint,
                info: Info {
                    line: self.line,
                    from: blueprint_from,
                    to: blueprint_to,
                },
            },
            StringInfo {
                name: alias,
                info: Info {
                    line: self.line,
                    from: alias_from,
                    to: alias_to,
                },
            },
        )
    }

    pub fn desugar_components(
        &mut self,
        scope: Vec<cst::ComponentScope>,
    ) -> Vec<(StringInfo, StringInfo)> {
        let mut components = vec![];
        let mut sep = true;
        self.col += 1; // {
        for c in scope {
            match c {
                cst::ComponentScope::Whitespace => self.desugar_whitespace(),
                cst::ComponentScope::Newline => self.desugar_newline(),
                cst::ComponentScope::ErrorTokens(err) => self.desugar_error(err),
                cst::ComponentScope::Comma => {
                    if sep {
                        self.errors.push(ErrorInfo {
                            error: Error::Unexpected {
                                expected: "component".into(),
                                token: Token::Comma,
                            },
                            info: Some(Info {
                                line: self.line,
                                from: self.col,
                                to: self.col + 1,
                            }),
                        })
                    }
                    sep = true;
                    self.col += 1;
                }
                cst::ComponentScope::Component(component) => {
                    if !sep {
                        self.errors.push(ErrorInfo {
                            error: Error::Missing {
                                expected: "`,`".into(),
                            },
                            info: Some(Info {
                                line: self.line,
                                from: self.col,
                                to: self.col + 1,
                            }),
                        })
                    }
                    sep = false;
                    components.push(self.desugar_component(component))
                }
            }
        }
        self.col += 1; // }
        components
    }

    pub fn desugar(mut self, cst: cst::Cst) -> ast::Ast {
        let mut last_automaton: Option<ast::Automaton> = None;
        let mut automata = vec![];
        for a in cst {
            match a {
                cst::AutomatonScope::Whitespace => self.desugar_whitespace(),
                cst::AutomatonScope::Newline | cst::AutomatonScope::LineComment(_) => {
                    self.desugar_newline()
                }
                cst::AutomatonScope::BlockComment(comment) => self.desugar_comment(comment),
                cst::AutomatonScope::ErrorTokens(err) => self.desugar_error(err),
                cst::AutomatonScope::Automaton { name, desc, w } => {
                    if let Some(automaton) = last_automaton.take() {
                        self.errors.push(ErrorInfo {
                            error: Error::Missing {
                                expected: "automaton states".into(),
                            },
                            info: Some(automaton.name.info),
                        });
                    }
                    self.col += 9 + w;
                    let from = self.col;
                    self.col += name.len() as u32;
                    last_automaton = Some(ast::Automaton {
                        name: StringInfo {
                            name,
                            info: Info {
                                line: self.line,
                                from,
                                to: self.col,
                            },
                        },
                        desc,
                        components: vec![],
                        states: vec![],
                    })
                }
                cst::AutomatonScope::Components(components) => {
                    if let Some(ref mut automaton) = last_automaton {
                        automaton
                            .components
                            .extend(self.desugar_components(components));
                    } else {
                        self.errors.push(ErrorInfo {
                            error: Error::Missing {
                                expected: "automaton declaration".into(),
                            },
                            info: Some(Info {
                                line: self.line,
                                from: self.col,
                                to: self.col + 1,
                            }),
                        });
                        self.desugar_components(components);
                    }
                }
                cst::AutomatonScope::States(states) => {
                    if let Some(mut automaton) = last_automaton.take() {
                        automaton.states.extend(self.desugar_states(states));
                        automata.push(automaton);
                    } else {
                        self.errors.push(ErrorInfo {
                            error: Error::Missing {
                                expected: "automaton declaration".into(),
                            },
                            info: Some(Info {
                                line: self.line,
                                from: self.col,
                                to: self.col + 1,
                            }),
                        });
                        self.desugar_states(states);
                    }
                }
            }
        }
        ast::Ast {
            automata,
            errors: self.errors,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::cst;
    use crate::token::Token::*;

    #[test]
    fn test_desugar_transition() {
        let mut desugarer = Desugarer::new();
        // A/B , R  -> add.first;
        let t = desugarer.desugar_transition(cst::Transition {
            read: 'A',
            write: 'B',
            mov: cst::Move::R,
            state: ("first".into(), Some("add".into())),
            w: [0, 0, 1, 1, 2, 1, 0].into(),
        });
        assert_eq!(
            t,
            ast::Transition {
                read: 'A',
                write: 'B',
                mov: ast::Move::R,
                state: (
                    StringInfo {
                        name: "first".into(),
                        info: Info {
                            line: 0,
                            from: 16,
                            to: 21
                        }
                    },
                    Some(StringInfo {
                        name: "add".into(),
                        info: Info {
                            line: 0,
                            from: 12,
                            to: 15
                        }
                    })
                )
            }
        )
    }

    #[test]
    fn test_desugar_transitions() {
        let mut desugarer = Desugarer::new();
        // {- Some
        // comment-} X / @, L -> q01; X / oops ;
        // 0/1,R -> q2 ;
        let t = desugarer.desugar_transitions(vec![
            cst::TransitionScope::BlockComment(" Some \ncomment".into()),
            cst::TransitionScope::Whitespace,
            cst::TransitionScope::Transition(cst::Transition {
                read: 'X',
                write: '@',
                mov: cst::Move::L,
                state: ("q01".into(), None),
                w: [1, 1, 0, 1, 1, 1, 0].into(),
            }),
            cst::TransitionScope::Whitespace,
            cst::TransitionScope::ErrorTokens(cst::ErrorTokens {
                error: Error::Unexpected {
                    expected: "symbol".into(),
                    token: Ident("oops".into(), "".into()),
                },
                location: Some(4),
                tokens: vec![
                    Symbol('X'),
                    Whitespace,
                    Slash,
                    Whitespace,
                    Ident("oops".into(), "".into()),
                    Whitespace,
                    Semicolon,
                ],
            }),
            cst::TransitionScope::Newline,
            cst::TransitionScope::Transition(cst::Transition {
                read: '0',
                write: '1',
                mov: cst::Move::R,
                state: ("q2".into(), None),
                w: [0, 0, 0, 0, 1, 1, 1].into(),
            }),
        ]);
        assert_eq!(
            t,
            vec![
                ast::Transition {
                    read: 'X',
                    write: '@',
                    mov: ast::Move::L,
                    state: (
                        StringInfo {
                            name: "q01".into(),
                            info: Info {
                                line: 1,
                                from: 22,
                                to: 25
                            }
                        },
                        None
                    )
                },
                ast::Transition {
                    read: '0',
                    write: '1',
                    mov: ast::Move::R,
                    state: (
                        StringInfo {
                            name: "q2".into(),
                            info: Info {
                                line: 2,
                                from: 9,
                                to: 11
                            }
                        },
                        None
                    )
                }
            ]
        );
        assert_eq!(
            desugarer.errors,
            vec![ErrorInfo {
                error: Error::Unexpected {
                    expected: "symbol".into(),
                    token: Ident("oops".into(), "".into())
                },
                info: Some(Info {
                    line: 1,
                    from: 31,
                    to: 35
                })
            }]
        )
    }

    #[test]
    fn test_desugar_final_state() {
        let mut desugarer = Desugarer::new();
        // accept state q0;
        let state = desugarer.desugar_final_state(cst::FinalState {
            accept: true,
            state: "q0".into(),
            desc: "none".into(),
            w: [1, 1, 0],
        });
        assert_eq!(
            state,
            ast::State {
                name: StringInfo {
                    name: "q0".into(),
                    info: Info {
                        line: 0,
                        from: 13,
                        to: 15
                    }
                },
                desc: "none".into(),
                typ: ast::StateType::Accept
            }
        );
    }

    #[test]
    fn test_desugar_arrow_state() {
        let mut desugarer = Desugarer::new();
        // state  first ->  add.second ;
        let state = desugarer.desugar_arrow_state(cst::ArrowState {
            initial: false,
            state: ("first".into(), None),
            new_state: ("second".into(), Some("add".into())),
            desc: "".into(),
            w: [0, 2, 1, 2, 1],
        });
        assert_eq!(
            state,
            ast::State {
                name: StringInfo {
                    name: "first".into(),
                    info: Info {
                        line: 0,
                        from: 7,
                        to: 12
                    }
                },
                desc: "".into(),
                typ: ast::StateType::State(
                    None,
                    false,
                    vec![ast::Transition {
                        read: '_',
                        write: '_',
                        mov: ast::Move::N,
                        state: (
                            StringInfo {
                                name: "second".into(),
                                info: Info {
                                    line: 0,
                                    from: 21,
                                    to: 27
                                }
                            },
                            Some(StringInfo {
                                name: "add".into(),
                                info: Info {
                                    line: 0,
                                    from: 17,
                                    to: 20
                                }
                            })
                        )
                    }]
                )
            }
        );
    }

    #[test]
    fn test_desugar_states() {
        let mut desugarer = Desugarer::new();
        // {state oops ; initial state abc ? {
        // huh ; A / B, R -> abc; woah
        // }
        let scope = vec![
            cst::StateScope::TransitionState(cst::TransitionState {
                initial: false,
                state: ("oops".into(), None),
                desc: "".into(),
                w: [0, 1, 1],
            }),
            cst::StateScope::ErrorTokens(cst::ErrorTokens {
                error: Error::Unexpected {
                    expected: "keyword `state`".into(),
                    token: Semicolon,
                },
                location: Some(0),
                tokens: vec![Semicolon],
            }),
            cst::StateScope::Whitespace,
            cst::StateScope::TransitionState(cst::TransitionState {
                initial: true,
                state: ("abc".into(), None),
                desc: "".into(),
                w: [1, 1, 1],
            }),
            cst::StateScope::ErrorTokens(cst::ErrorTokens {
                error: Error::Unexpected {
                    expected: "keyword `state`".into(),
                    token: Unknown('?'),
                },
                location: Some(0),
                tokens: vec![Unknown('?'), Whitespace],
            }),
            cst::StateScope::Transitions(vec![
                cst::TransitionScope::Newline,
                cst::TransitionScope::ErrorTokens(cst::ErrorTokens {
                    error: Error::Unexpected {
                        expected: "symbol".into(),
                        token: Ident("huh".into(), "".into()),
                    },
                    location: Some(0),
                    tokens: vec![Ident("huh".into(), "".into()), Whitespace, Semicolon],
                }),
                cst::TransitionScope::Whitespace,
                cst::TransitionScope::Transition(cst::Transition {
                    read: 'A',
                    write: 'B',
                    mov: cst::Move::R,
                    state: ("abc".into(), None),
                    w: [1, 1, 0, 1, 1, 1, 0],
                }),
                cst::TransitionScope::Whitespace,
                cst::TransitionScope::ErrorTokens(cst::ErrorTokens {
                    error: Error::Unexpected {
                        expected: "symbol".into(),
                        token: Ident("woah".into(), "".into()),
                    },
                    location: Some(0),
                    tokens: vec![Ident("woah".into(), "".into()), Whitespace, Semicolon],
                }),
                cst::TransitionScope::Newline,
            ]),
        ];
        let states = desugarer.desugar_states(scope);
        assert_eq!(
            states,
            vec![
                ast::State {
                    name: StringInfo {
                        name: "oops".into(),
                        info: Info {
                            line: 0,
                            from: 7,
                            to: 11
                        }
                    },
                    typ: ast::StateType::State(None, false, vec![]),
                    desc: "".into()
                },
                ast::State {
                    name: StringInfo {
                        name: "abc".into(),
                        info: Info {
                            line: 0,
                            from: 28,
                            to: 31
                        }
                    },
                    typ: ast::StateType::State(
                        None,
                        true,
                        vec![ast::Transition {
                            read: 'A',
                            write: 'B',
                            mov: ast::Move::R,
                            state: (
                                StringInfo {
                                    name: "abc".into(),
                                    info: Info {
                                        line: 1,
                                        from: 18,
                                        to: 21
                                    }
                                },
                                None
                            )
                        }]
                    ),
                    desc: "".into()
                }
            ]
        );
        assert_eq!(
            desugarer.errors,
            vec![
                ErrorInfo {
                    error: Error::Unexpected {
                        expected: "keyword `state`".into(),
                        token: Semicolon
                    },
                    info: Some(Info {
                        line: 0,
                        from: 12,
                        to: 13
                    })
                },
                ErrorInfo {
                    error: Error::Missing {
                        expected: "state transitions".into()
                    },
                    info: Some(Info {
                        line: 0,
                        from: 1,
                        to: 12
                    })
                },
                ErrorInfo {
                    error: Error::Unexpected {
                        expected: "keyword `state`".into(),
                        token: Unknown('?')
                    },
                    info: Some(Info {
                        line: 0,
                        from: 32,
                        to: 33
                    })
                },
                ErrorInfo {
                    error: Error::Unexpected {
                        expected: "symbol".into(),
                        token: Ident("huh".into(), "".into())
                    },
                    info: Some(Info {
                        line: 1,
                        from: 0,
                        to: 3
                    })
                },
                ErrorInfo {
                    error: Error::Unexpected {
                        expected: "symbol".into(),
                        token: Ident("woah".into(), "".into())
                    },
                    info: Some(Info {
                        line: 1,
                        from: 23,
                        to: 27
                    })
                }
            ]
        );
    }

    #[test]
    fn test_automaton_desugar() {
        let desugarer = Desugarer::new();
        // -- Some comment
        // automaton main huh (x as y) what
        // {{- First state -}
        //   accept state ok; oops ?
        // }
        let cst = vec![
            cst::AutomatonScope::LineComment(" Some comment\n".into()),
            cst::AutomatonScope::Automaton {
                name: "main".into(),
                desc: " Some comment\n".into(),
                w: 1,
            },
            cst::AutomatonScope::Whitespace,
            cst::AutomatonScope::ErrorTokens(cst::ErrorTokens {
                error: Error::Unexpected {
                    expected: "keyword `automaton`".into(),
                    token: Ident("huh".into(), "".into()),
                },
                location: Some(0),
                tokens: vec![Ident("huh".into(), "".into()), Whitespace],
            }),
            cst::AutomatonScope::Components(vec![cst::ComponentScope::Component(cst::Component {
                blueprint: "x".into(),
                alias: "y".into(),
                w: [1, 1],
            })]),
            cst::AutomatonScope::ErrorTokens(cst::ErrorTokens {
                error: Error::Unexpected {
                    expected: "keyword `automaton`".into(),
                    token: Ident("what".into(), "".into()),
                },
                location: Some(0),
                tokens: vec![Ident("what".into(), "".into())],
            }),
            cst::AutomatonScope::Newline,
            cst::AutomatonScope::States(vec![
                cst::StateScope::BlockComment(" First state ".into()),
                cst::StateScope::Newline,
                cst::StateScope::Whitespace,
                cst::StateScope::Whitespace,
                cst::StateScope::FinalState(cst::FinalState {
                    accept: true,
                    state: "ok".into(),
                    desc: " First state ".into(),
                    w: [1, 1, 0],
                }),
                cst::StateScope::ErrorTokens(cst::ErrorTokens {
                    error: Error::Unexpected {
                        expected: "keyword `state`".into(),
                        token: Ident("oops".into(), "".into()),
                    },
                    location: Some(0),
                    tokens: vec![Ident("oops".into(), "".into()), Whitespace, Unknown('?')],
                }),
                cst::StateScope::Newline,
            ]),
        ];
        let ast = desugarer.desugar(cst);
        assert_eq!(
            ast,
            ast::Ast {
                automata: vec![ast::Automaton {
                    name: StringInfo {
                        name: "main".into(),
                        info: Info {
                            line: 1,
                            from: 10,
                            to: 14
                        }
                    },
                    components: vec![(
                        StringInfo {
                            name: "x".into(),
                            info: Info {
                                line: 1,
                                from: 20,
                                to: 21
                            }
                        },
                        StringInfo {
                            name: "y".into(),
                            info: Info {
                                line: 1,
                                from: 25,
                                to: 26
                            }
                        }
                    )],
                    states: vec![ast::State {
                        name: StringInfo {
                            name: "ok".into(),
                            info: Info {
                                line: 3,
                                from: 15,
                                to: 17
                            }
                        },
                        typ: ast::StateType::Accept,
                        desc: " First state ".into()
                    }],
                    desc: " Some comment\n".into()
                }],
                errors: vec![
                    ErrorInfo {
                        error: Error::Unexpected {
                            expected: "keyword `automaton`".into(),
                            token: Ident("huh".into(), "".into())
                        },
                        info: Some(Info {
                            line: 1,
                            from: 15,
                            to: 18
                        })
                    },
                    ErrorInfo {
                        error: Error::Unexpected {
                            expected: "keyword `automaton`".into(),
                            token: Ident("what".into(), "".into())
                        },
                        info: Some(Info {
                            line: 1,
                            from: 27,
                            to: 31
                        })
                    },
                    ErrorInfo {
                        error: Error::Unexpected {
                            expected: "keyword `state`".into(),
                            token: Ident("oops".into(), "".into())
                        },
                        info: Some(Info {
                            line: 3,
                            from: 18,
                            to: 22
                        })
                    }
                ]
            }
        )
    }
}
