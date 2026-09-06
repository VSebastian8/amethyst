use std::rc::Rc;

use crate::cst;
use crate::cst::AutomatonScope;
use crate::cst::ErrorTokens;
use crate::info::*;
use crate::token::Token;
use crate::token::Token::RBracket;
use crate::token::Token::RParanthesis;
pub struct Parser {
    tokens: Vec<Token>,
    pub errors: Vec<Error>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            errors: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), Error> {
        match self.advance() {
            Some(t) if *t == expected => Ok(()),
            Some(token) => Err(Error::Unexpected {
                token: token.clone(),
                expected: expected.debug(),
            }),
            None => Err(Error::EOF {
                expected: expected.debug(),
            }),
        }
    }

    fn finalize(&mut self, expected: Token) -> Result<(), Error> {
        match self.advance() {
            Some(t) if *t == expected => Ok(()),
            Some(_) => Err(Error::Missing {
                expected: expected.debug(),
            }),
            None => Err(Error::EOF {
                expected: expected.debug(),
            }),
        }
    }

    fn recover(&mut self, start: usize, until: &[Token], before: &[Token]) -> Vec<Token> {
        self.pos = start;
        let mut tokens = vec![];
        while !self.is_at_end() {
            let current = self.pos;
            if before.contains(&self.tokens[current]) {
                break;
            }
            tokens.push(self.tokens[current].clone());
            self.advance();
            if until.contains(&self.tokens[current]) {
                break;
            }
        }
        tokens
    }

    fn parse_whitespace(&mut self) -> u32 {
        let mut w = 0;
        while Some(&Token::Whitespace) == self.peek() {
            self.advance();
            w += 1;
        }
        w
    }

    fn parse_symbol(&mut self) -> Result<char, Error> {
        match self.advance() {
            Some(Token::Newline) => Err(Error::Missing {
                expected: "symbol".into(),
            }),
            Some(Token::Symbol(ch)) => Ok(*ch),
            Some(token) => Err(Error::Unexpected {
                token: token.clone(),
                expected: "symbol".into(),
            }),
            None => Err(Error::EOF {
                expected: "symbol".into(),
            }),
        }
    }

    fn parse_move(&mut self) -> Result<cst::Move, Error> {
        match self.advance() {
            Some(Token::Newline) => Err(Error::Missing {
                expected: "move symbol".into(),
            }),
            Some(Token::Symbol(ch)) => match ch {
                'L' => Ok(cst::Move::L),
                'R' => Ok(cst::Move::R),
                'N' => Ok(cst::Move::N),
                _ => Err(Error::Unexpected {
                    token: Token::Symbol(*ch),
                    expected: "move symbol".into(),
                }),
            },
            Some(token) => Err(Error::Unexpected {
                token: token.clone(),
                expected: "move symbol".into(),
            }),
            None => Err(Error::EOF {
                expected: "move symbol".into(),
            }),
        }
    }

    fn parse_ident(&mut self) -> Result<(Rc<str>, Rc<str>), Error> {
        match self.advance() {
            Some(Token::Newline) => Err(Error::Missing {
                expected: "identifier".into(),
            }),
            Some(token) => match &token {
                Token::Ident(name, _)
                    if name.chars().any(|c: char| {
                        !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_'
                    }) =>
                {
                    Err(Error::MalformedIdentifier {
                        ident: name.clone(),
                    })
                }
                Token::Ident(name, description) => Ok((name.clone(), description.clone())),
                _ => Err(Error::Unexpected {
                    token: token.clone(),
                    expected: "identifier".into(),
                }),
            },
            None => Err(Error::EOF {
                expected: "identifier".into(),
            }),
        }
    }

    fn parse_state_name(&mut self) -> Result<(Rc<str>, Option<Rc<str>>, Rc<str>), Error> {
        let (name, desc) = self.parse_ident()?;
        match self.peek() {
            Some(Token::Dot) => {
                self.advance();
                Ok((self.parse_ident()?.0, Some(name), desc))
            }
            _ => Ok((name, None, desc)),
        }
    }

    fn parse_transition(&mut self) -> Result<cst::Transition, Error> {
        let read = self.parse_symbol()?;
        let w1 = self.parse_whitespace();
        self.expect(Token::Slash)?;
        let w2 = self.parse_whitespace();
        let write = self.parse_symbol()?;
        let w3 = self.parse_whitespace();
        self.expect(Token::Comma)?;
        let w4 = self.parse_whitespace();
        let mov = self.parse_move()?;
        let w5 = self.parse_whitespace();
        self.expect(Token::Arrow)?;
        let w6 = self.parse_whitespace();
        let (state, parent, _) = self.parse_state_name()?;
        let w7 = self.parse_whitespace();
        self.finalize(Token::Semicolon)?;

        Ok(cst::Transition {
            read,
            write,
            mov,
            state: (state, parent),
            w: [w1, w2, w3, w4, w5, w6, w7].into(),
        })
    }

    fn parse_transition_scope(&mut self) -> cst::TransitionScope {
        let start = self.pos;
        match self.advance() {
            None => cst::TransitionScope::ErrorTokens(ErrorTokens {
                error: Error::EOF {
                    expected: "`}`".into(),
                },
                location: None,
                tokens: Vec::new(),
            }),
            Some(token) => match token {
                Token::Whitespace => cst::TransitionScope::Whitespace,
                Token::Newline => cst::TransitionScope::Newline,
                Token::LineComment(msg) => cst::TransitionScope::LineComment(msg.clone()),
                Token::BlockComment(msg) => cst::TransitionScope::BlockComment(msg.clone()),
                Token::LBracket => cst::TransitionScope::ErrorTokens(ErrorTokens {
                    error: Error::NotAllowed {
                        reason: "Nested scope in transitions".into(),
                    },
                    location: Some(0),
                    tokens: self.recover(start, &[Token::RBracket], &[]),
                }),
                _ => {
                    self.pos -= 1;
                    match self.parse_transition() {
                        Ok(transition) => cst::TransitionScope::Transition(transition),
                        Err(error) => cst::TransitionScope::ErrorTokens(ErrorTokens {
                            error,
                            location: Some(self.pos - start - 1),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        }),
                    }
                }
            },
        }
    }

    fn parse_transitions(&mut self) -> Vec<cst::TransitionScope> {
        // Parse state transitions until }
        let mut transitions = Vec::new();
        loop {
            match self.peek() {
                None => {
                    transitions.push(cst::TransitionScope::ErrorTokens(ErrorTokens {
                        error: Error::EOF {
                            expected: "`}`".into(),
                        },
                        location: None,
                        tokens: vec![],
                    }));
                    break;
                }
                Some(&Token::RBracket) => {
                    self.advance();
                    break;
                }
                _ => {
                    transitions.push(self.parse_transition_scope());
                }
            }
        }
        transitions
    }

    fn parse_final_state(&mut self) -> Result<cst::FinalState, Error> {
        let accept = match self.peek() {
            Some(Token::Accept) => true,
            Some(Token::Reject) => false,
            Some(token) => {
                return Err(Error::Unexpected {
                    expected: "final state".into(),
                    token: token.clone(),
                })
            }
            None => {
                return Err(Error::EOF {
                    expected: "final state".into(),
                })
            }
        };
        self.advance();
        let w1 = self.parse_whitespace();
        self.expect(Token::State)?;
        let w2 = self.parse_whitespace();
        let (state, desc) = self.parse_ident()?;
        let w3 = self.parse_whitespace();
        self.finalize(Token::Semicolon)?;
        Ok(cst::FinalState {
            accept,
            state,
            desc,
            w: [w1, w2, w3].into(),
        })
    }

    // Transition / Arrow state
    fn parse_transition_state(&mut self) -> Result<cst::StateScope, Error> {
        let initial = match self.peek() {
            Some(&Token::Initial) => {
                self.advance();
                true
            }
            _ => false,
        };
        let w1 = self.parse_whitespace();
        self.expect(Token::State)?;
        let w2 = self.parse_whitespace();
        let (state, parent, desc) = self.parse_state_name()?;
        let w3 = self.parse_whitespace();
        match self.peek() {
            Some(&Token::Arrow) => {
                // Arrow state
                self.advance();
                let w4 = self.parse_whitespace();
                let (new_state, new_parent, _) = self.parse_state_name()?;
                let w5 = self.parse_whitespace();
                self.finalize(Token::Semicolon)?;
                Ok(cst::StateScope::ArrowState(cst::ArrowState {
                    initial,
                    state: (state, parent),
                    new_state: (new_state, new_parent),
                    desc,
                    w: [w1, w2, w3, w4, w5].into(),
                }))
            }
            _ => Ok(cst::StateScope::TransitionState(cst::TransitionState {
                initial,
                state: (state, parent),
                desc,
                w: [w1, w2, w3].into(),
            })),
        }
    }

    fn parse_state_scope(&mut self) -> cst::StateScope {
        let start = self.pos;
        match self.advance() {
            None => cst::StateScope::ErrorTokens(ErrorTokens {
                error: Error::EOF {
                    expected: "`}`".into(),
                },
                location: None,
                tokens: Vec::new(),
            }),
            Some(token) => match token {
                Token::Whitespace => cst::StateScope::Whitespace,
                Token::Newline => cst::StateScope::Newline,
                Token::LineComment(msg) => cst::StateScope::LineComment(msg.clone()),
                Token::BlockComment(msg) => cst::StateScope::BlockComment(msg.clone()),
                Token::LBracket => cst::StateScope::Transitions(self.parse_transitions()),
                Token::Accept | Token::Reject => {
                    self.pos -= 1;
                    match self.parse_final_state() {
                        Ok(state) => cst::StateScope::FinalState(state),
                        Err(error) => cst::StateScope::ErrorTokens(ErrorTokens {
                            error,
                            location: Some(self.pos - start - 1),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        }),
                    }
                }
                _ => {
                    self.pos -= 1;
                    match self.parse_transition_state() {
                        Ok(state) => state,
                        Err(error) => cst::StateScope::ErrorTokens(ErrorTokens {
                            error,
                            location: Some(self.pos - start - 1),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        }),
                    }
                }
            },
        }
    }

    fn parse_states(&mut self) -> Vec<cst::StateScope> {
        // Parse automata states until }
        let mut states = Vec::new();
        loop {
            match self.peek() {
                None => {
                    states.push(cst::StateScope::ErrorTokens(ErrorTokens {
                        error: Error::EOF {
                            expected: "`}`".into(),
                        },
                        location: None,
                        tokens: vec![],
                    }));
                    break;
                }
                Some(&Token::RBracket) => {
                    self.advance();
                    break;
                }
                _ => {
                    states.push(self.parse_state_scope());
                }
            }
        }
        states
    }

    fn parse_component(&mut self) -> Result<cst::Component, Error> {
        let blueprint = self.parse_ident()?.0;
        let w1 = self.parse_whitespace();
        self.expect(Token::As)?;
        let w2 = self.parse_whitespace();
        let alias = self.parse_ident()?.0;
        Ok(cst::Component {
            blueprint,
            alias,
            w: [w1, w2].into(),
        })
    }

    fn parse_component_scope(&mut self) -> cst::ComponentScope {
        let start = self.pos;
        match self.advance() {
            None => cst::ComponentScope::ErrorTokens(ErrorTokens {
                error: Error::EOF {
                    expected: "`)`".into(),
                },
                location: None,
                tokens: Vec::new(),
            }),
            Some(token) => match token {
                Token::Whitespace => cst::ComponentScope::Whitespace,
                Token::Newline => cst::ComponentScope::Newline,
                Token::LineComment(_) | Token::BlockComment(_) => {
                    cst::ComponentScope::ErrorTokens(ErrorTokens {
                        error: Error::NotAllowed {
                            reason: "comment inside component list".into(),
                        },
                        location: None,
                        tokens: vec![token.clone()],
                    })
                }
                Token::Comma => cst::ComponentScope::Comma,
                _ => {
                    self.pos -= 1;
                    match self.parse_component() {
                        Ok(component) => cst::ComponentScope::Component(component),
                        Err(error) => cst::ComponentScope::ErrorTokens(ErrorTokens {
                            error,
                            location: Some(self.pos - start - 1),
                            tokens: self.recover(
                                start,
                                &[Token::Comma],
                                &[
                                    Token::Newline,
                                    Token::LBracket,
                                    Token::RBracket,
                                    Token::LParanthesis,
                                    Token::RParanthesis,
                                ],
                            ),
                        }),
                    }
                }
            },
        }
    }

    fn parse_components(&mut self) -> Vec<cst::ComponentScope> {
        // Parse automata components until ) / {
        let mut components = Vec::new();
        loop {
            match self.peek() {
                None => {
                    components.push(cst::ComponentScope::ErrorTokens(ErrorTokens {
                        error: Error::EOF {
                            expected: "`)`".into(),
                        },
                        location: None,
                        tokens: vec![],
                    }));
                    break;
                }
                Some(&Token::LBracket) => {
                    components.push(cst::ComponentScope::ErrorTokens(ErrorTokens {
                        error: Error::Missing {
                            expected: "`)`".into(),
                        },
                        location: None,
                        tokens: vec![],
                    }));
                    break;
                }
                Some(&Token::RParanthesis) => {
                    self.advance();
                    break;
                }
                _ => {
                    components.push(self.parse_component_scope());
                }
            }
        }
        components
    }

    fn parse_automaton(&mut self) -> Result<AutomatonScope, Error> {
        self.expect(Token::Automaton)?;
        let w = self.parse_whitespace();
        let (name, desc) = self.parse_ident()?;
        Ok(cst::AutomatonScope::Automaton { name, desc, w })
    }

    fn parse_automaton_scope(&mut self) -> AutomatonScope {
        let start = self.pos;
        match self.advance() {
            None => cst::AutomatonScope::ErrorTokens(ErrorTokens {
                error: Error::EOF {
                    expected: "automaton declaration".into(),
                },
                location: None,
                tokens: Vec::new(),
            }),
            Some(token) => match token {
                Token::Whitespace => cst::AutomatonScope::Whitespace,
                Token::Newline => cst::AutomatonScope::Newline,
                Token::LineComment(x) => cst::AutomatonScope::LineComment(x.clone()),
                Token::BlockComment(x) => cst::AutomatonScope::BlockComment(x.clone()),
                Token::LParanthesis => cst::AutomatonScope::Components(self.parse_components()),
                Token::LBracket => cst::AutomatonScope::States(self.parse_states()),
                RParanthesis => cst::AutomatonScope::ErrorTokens(ErrorTokens {
                    error: Error::Unexpected {
                        expected: "automata declaration".into(),
                        token: RParanthesis,
                    },
                    location: Some(0),
                    tokens: vec![RParanthesis],
                }),
                RBracket => cst::AutomatonScope::ErrorTokens(ErrorTokens {
                    error: Error::Unexpected {
                        expected: "automata declaration".into(),
                        token: RBracket,
                    },
                    location: Some(0),
                    tokens: vec![RBracket],
                }),
                _ => {
                    self.pos -= 1;
                    match self.parse_automaton() {
                        Ok(automaton) => automaton,
                        Err(error) => cst::AutomatonScope::ErrorTokens(ErrorTokens {
                            error,
                            location: Some(self.pos - start - 1),
                            tokens: self.recover(
                                start,
                                &[],
                                &[
                                    Token::Newline,
                                    Token::LBracket,
                                    Token::RBracket,
                                    Token::LParanthesis,
                                    Token::RParanthesis,
                                ],
                            ),
                        }),
                    }
                }
            },
        }
    }

    pub fn parse(mut self) -> cst::Cst {
        // Parse automata until EOF
        let mut automata = Vec::new();
        loop {
            match self.peek() {
                None => {
                    break;
                }
                _ => {
                    automata.push(self.parse_automaton_scope());
                }
            }
        }
        automata
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cst::*;
    use Token::*;

    #[test]
    fn test_parse_transition() {
        let tokens = vec![
            Symbol('A'),
            Whitespace,
            Slash,
            Symbol('B'),
            Whitespace,
            Whitespace,
            Comma,
            Symbol('L'),
            Arrow,
            Ident("s2".into(), "".into()),
            Whitespace,
            Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let t = parser.parse_transition().unwrap();

        assert_eq!(
            t,
            Transition {
                read: 'A',
                write: 'B',
                mov: Move::L,
                state: ("s2".into(), None),
                w: [1, 0, 2, 0, 0, 0, 1].into()
            }
        );
    }

    #[test]
    fn test_parse_transition_error() {
        let tokens = vec![
            Symbol('A'),
            Slash,
            Ident("bye".into(), "".into()),
            Comma,
            Symbol('L'),
            Arrow,
            Ident("s2".into(), "".into()),
            Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let err = parser.parse_transition().unwrap_err();

        assert_eq!(
            err,
            Error::Unexpected {
                expected: "symbol".into(),
                token: Ident("bye".into(), "".into())
            }
        );
    }

    #[test]
    fn test_parse_state() {
        let tokens = vec![
            Initial,
            Whitespace,
            State,
            Whitespace,
            Ident("first".into(), "this is the initial state".into()),
            Newline,
            LBracket,
            Symbol('_'),
            Slash,
            Symbol('@'),
            Comma,
            Symbol('R'),
            Arrow,
            Ident("add".into(), "".into()),
            Dot,
            Ident("input".into(), "".into()),
            Semicolon,
            RBracket,
            RBracket,
        ];
        let mut parser = Parser::new(tokens);
        let s = parser.parse_states();

        assert_eq!(
            s,
            vec![
                StateScope::TransitionState(TransitionState {
                    initial: true,
                    state: ("first".into(), None),
                    desc: "this is the initial state".into(),
                    w: [1, 1, 0].into()
                }),
                StateScope::Newline,
                StateScope::Transitions(vec![TransitionScope::Transition(Transition {
                    read: '_',
                    write: '@',
                    mov: Move::R,
                    state: ("input".into(), Some("add".into())),
                    w: [0, 0, 0, 0, 0, 0, 0].into()
                })])
            ]
        );
    }

    #[test]
    fn test_arrow_state() {
        let tokens = vec![
            State,
            Whitespace,
            Whitespace,
            Ident("x".into(), "could be y".into()),
            Dot,
            Ident("some_name".into(), "".into()),
            Whitespace,
            Arrow,
            Ident("y12".into(), "".into()),
            Dot,
            Ident("some_name2".into(), "".into()),
            Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let s = parser.parse_transition_state().unwrap();

        assert_eq!(
            s,
            StateScope::ArrowState(ArrowState {
                initial: false,
                state: ("some_name".into(), Some("x".into())),
                new_state: ("some_name2".into(), Some("y12".into())),
                desc: "could be y".into(),
                w: [0, 2, 1, 0, 0].into()
            })
        );
    }

    #[test]
    fn test_final_states() {
        let tokens = vec![
            Accept,
            State,
            Whitespace,
            Whitespace,
            Whitespace,
            Ident("done".into(), "final state".into()),
            Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let s = parser.parse_final_state().unwrap();

        assert_eq!(
            s,
            FinalState {
                accept: true,
                state: "done".into(),
                desc: "final state".into(),
                w: [0, 3, 0].into()
            }
        );

        let tokens = vec![
            Reject,
            State,
            Ident("over".into(), "".into()),
            Whitespace,
            Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let s = parser.parse_final_state().unwrap();

        assert_eq!(
            s,
            FinalState {
                accept: false,
                state: "over".into(),
                desc: "".into(),
                w: [0, 0, 1].into()
            }
        );
    }

    #[test]
    fn test_parse_state_error() {
        let tokens = vec![
            Newline,
            State,
            Whitespace,
            Arrow,
            Ident("some".into(), "".into()),
            RBracket,
        ];
        let mut parser = Parser::new(tokens);
        let scope = parser.parse_states();
        assert_eq!(
            scope,
            vec![
                StateScope::Newline,
                StateScope::ErrorTokens(ErrorTokens {
                    error: Error::Unexpected {
                        expected: "identifier".into(),
                        token: Arrow
                    },
                    location: Some(2),
                    tokens: vec![State, Whitespace, Arrow, Ident("some".into(), "".into())]
                })
            ]
        );

        let tokens = vec![
            State,
            Ident("some".into(), "".into()),
            Arrow,
            Ident("other".into(), "".into()),
            RBracket,
        ];
        let mut parser = Parser::new(tokens);
        let scope = parser.parse_states();
        assert_eq!(
            scope,
            vec![StateScope::ErrorTokens(ErrorTokens {
                error: Error::Missing {
                    expected: "`;`".into(),
                },
                location: Some(4),
                tokens: vec![
                    State,
                    Ident("some".into(), "".into()),
                    Arrow,
                    Ident("other".into(), "".into())
                ]
            })]
        );
    }

    #[test]
    fn test_parse_components() {
        let tokens = vec![
            Ident("some".into(), "".into()),
            Whitespace,
            As,
            Whitespace,
            Whitespace,
            Ident("other".into(), "".into()),
            Comma,
            Newline,
            Whitespace,
            Ident("a".into(), "".into()),
            Whitespace,
            As,
            Symbol('X'),
            Automaton,
            State,
            RParanthesis,
        ];
        let mut parser = Parser::new(tokens);
        let scope = parser.parse_components();
        assert_eq!(
            scope,
            vec![
                ComponentScope::Component(Component {
                    blueprint: "some".into(),
                    alias: "other".into(),
                    w: [1, 2].into()
                }),
                ComponentScope::Comma,
                ComponentScope::Newline,
                ComponentScope::Whitespace,
                ComponentScope::ErrorTokens(ErrorTokens {
                    error: Error::Unexpected {
                        expected: "identifier".into(),
                        token: Symbol('X')
                    },
                    location: Some(3),
                    tokens: vec![
                        Ident("a".into(), "".into()),
                        Whitespace,
                        As,
                        Symbol('X'),
                        Automaton,
                        State,
                    ]
                })
            ]
        )
    }

    #[test]
    fn test_automaton() {
        let tokens = vec![
            Automaton,
            Whitespace,
            Whitespace,
            Ident("main".into(), "entry\npoint".into()),
            Newline,
            LParanthesis,
            Ident("add".into(), "".into()),
            As,
            Ident("a1".into(), "".into()),
            Comma,
            Ident("other_auto".into(), "".into()),
            As,
            Ident("unused".into(), "".into()),
            RParanthesis,
            LBracket,
            Initial,
            State,
            Ident("start".into(), "first state".into()),
            LBracket,
            Symbol('_'),
            Slash,
            Symbol('0'),
            Comma,
            Symbol('N'),
            Arrow,
            Ident("a1".into(), "".into()),
            Dot,
            Ident("input".into(), "".into()),
            Semicolon,
            RBracket,
            State,
            Ident("a1".into(), "final state \n of a \n component".into()),
            Dot,
            Ident("output".into(), "".into()),
            Arrow,
            Ident("done".into(), "".into()),
            Semicolon,
            Accept,
            State,
            Ident("done".into(), "".into()),
            Semicolon,
            RBracket,
        ];
        let parser = Parser::new(tokens);
        let scope = parser.parse();
        assert_eq!(
            scope,
            vec![
                AutomatonScope::Automaton {
                    name: "main".into(),
                    desc: "entry\npoint".into(),
                    w: 2
                },
                AutomatonScope::Newline,
                AutomatonScope::Components(vec![
                    ComponentScope::Component(Component {
                        blueprint: "add".into(),
                        alias: "a1".into(),
                        w: [0, 0].into()
                    }),
                    ComponentScope::Comma,
                    ComponentScope::Component(Component {
                        blueprint: "other_auto".into(),
                        alias: "unused".into(),
                        w: [0, 0].into()
                    })
                ]),
                AutomatonScope::States(vec![
                    StateScope::TransitionState(TransitionState {
                        initial: true,
                        state: ("start".into(), None),
                        desc: "first state".into(),
                        w: [0, 0, 0].into()
                    }),
                    StateScope::Transitions(vec![TransitionScope::Transition(Transition {
                        read: '_',
                        write: '0',
                        mov: Move::N,
                        state: ("input".into(), Some("a1".into())),
                        w: [0, 0, 0, 0, 0, 0, 0].into()
                    })]),
                    StateScope::ArrowState(ArrowState {
                        initial: false,
                        state: ("output".into(), Some("a1".into())),
                        new_state: ("done".into(), None),
                        desc: "final state \n of a \n component".into(),
                        w: [0, 0, 0, 0, 0].into()
                    }),
                    StateScope::FinalState(FinalState {
                        accept: true,
                        state: "done".into(),
                        desc: "".into(),
                        w: [0, 0, 0].into()
                    })
                ])
            ]
        );
    }

    #[test]
    fn test_error_components() {
        let tokens = vec![
            Automaton,
            Ident("ok".into(), "".into()),
            LParanthesis,
            Ident("place".into(), "".into()),
            Ident("xor".into(), "".into()),
            Comma,
            Ident("good".into(), "".into()),
            As,
            Ident("dea?d".into(), "".into()),
            Comma,
            RParanthesis,
            LBracket,
            RBracket,
        ];
        let parser = Parser::new(tokens);
        let scope = parser.parse();
        assert_eq!(
            scope,
            vec![
                AutomatonScope::Automaton {
                    name: "ok".into(),
                    desc: "".into(),
                    w: 0
                },
                AutomatonScope::Components(vec![
                    ComponentScope::ErrorTokens(ErrorTokens {
                        error: Error::Unexpected {
                            expected: "keyword `as`".into(),
                            token: Ident("xor".into(), "".into())
                        },
                        location: Some(1),
                        tokens: vec![
                            Ident("place".into(), "".into()),
                            Ident("xor".into(), "".into()),
                            Comma
                        ]
                    }),
                    ComponentScope::ErrorTokens(ErrorTokens {
                        error: Error::MalformedIdentifier {
                            ident: "dea?d".into()
                        },
                        location: Some(2),
                        tokens: vec![
                            Ident("good".into(), "".into()),
                            As,
                            Ident("dea?d".into(), "".into()),
                            Comma
                        ]
                    })
                ]),
                AutomatonScope::States(vec![])
            ]
        );
    }
}
