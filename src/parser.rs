use std::rc::Rc;

use crate::ast;
use crate::cst;
use crate::info::*;
use crate::token::Token;
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
        match self.peek() {
            Some(t) if *t == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(Error::Unexpected {
                token: token.clone(),
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

    fn parse_whitespace(&mut self) -> usize {
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
        match self.peek() {
            Some(Token::Newline) => Err(Error::Missing {
                expected: "identifier".into(),
            }),
            Some(token) => match &token {
                Token::Ident(name, description) => {
                    let res = (name.clone(), description.clone());
                    self.advance();
                    Ok(res)
                }
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
        self.expect(Token::Semicolon)?;

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
            None => cst::TransitionScope::ErrorTokens {
                error: Error::EOF {
                    expected: "`}`".into(),
                },
                location: None,
                tokens: Vec::new(),
            },
            Some(token) => match token {
                Token::Whitespace => cst::TransitionScope::Whitespace,
                Token::Newline => cst::TransitionScope::Newline,
                Token::LineComment(msg) => cst::TransitionScope::LineComment(msg.clone()),
                Token::BlockComment(msg) => cst::TransitionScope::BlockComment(msg.clone()),
                Token::LBracket => cst::TransitionScope::ErrorTokens {
                    error: Error::NotAllowed {
                        reason: "Nested scope in transitions".into(),
                    },
                    location: Some(0),
                    tokens: self.recover(start, &[Token::RBracket], &[]),
                },
                _ => {
                    self.pos -= 1;
                    match self.parse_transition() {
                        Ok(transition) => cst::TransitionScope::Transition(transition),
                        Err(error) => cst::TransitionScope::ErrorTokens {
                            error,
                            location: Some(self.pos - start),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        },
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
                None | Some(&Token::RBracket) => {
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
        self.expect(Token::Semicolon)?;
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
                self.expect(Token::Semicolon)?;
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
            None => cst::StateScope::ErrorTokens {
                error: Error::EOF {
                    expected: "`}`".into(),
                },
                location: None,
                tokens: Vec::new(),
            },
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
                        Err(error) => cst::StateScope::ErrorTokens {
                            error,
                            location: Some(self.pos - start),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        },
                    }
                }
                _ => {
                    self.pos -= 1;
                    match self.parse_transition_state() {
                        Ok(state) => state,
                        Err(error) => cst::StateScope::ErrorTokens {
                            error,
                            location: Some(self.pos - start),
                            tokens: self.recover(
                                start,
                                &[Token::Semicolon],
                                &[Token::Newline, Token::LBracket, Token::RBracket],
                            ),
                        },
                    }
                }
            },
        }
    }

    fn parse_states(&mut self) -> Vec<cst::StateScope> {
        // Parse state transitions until }
        let mut states = Vec::new();
        loop {
            match self.peek() {
                None | Some(&Token::RBracket) => {
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

    // fn parse_component(&mut self) -> Result<(StringInfo, StringInfo), Error> {
    //     let line = self.get_line();
    //     let path = self.parse_ident(line)?.0;
    //     self.expect_on_line(Token::As, line)?;
    //     let name = self.parse_ident(line)?.0;
    //     Ok((path, name))
    // }

    // fn parse_components(&mut self) -> Vec<(StringInfo, StringInfo)> {
    //     let mut components = Vec::new();
    //     let mut recovered = false;
    //     loop {
    //         match self.peek_token() {
    //             None => break,
    //             Some(&Token::LBracket) => {
    //                 self.errors.push(Error::Missing {
    //                     expected: "`)`".into(),
    //                     info: self.get_info(),
    //                 });
    //                 break;
    //             }
    //             Some(&Token::RParanthesis) => {
    //                 self.advance();
    //                 break;
    //             }
    //             _ => {
    //                 let sep = if components.len() > 0 && !recovered {
    //                     self.expect(Token::Comma)
    //                 } else {
    //                     Ok(())
    //                 };
    //                 match sep.and(self.parse_component()) {
    //                     Ok(c) => {
    //                         recovered = false;
    //                         components.push(c)
    //                     }
    //                     Err(err) => {
    //                         self.errors.push(err);
    //                         let line = self.get_line();
    //                         // Error recovery
    //                         // Advance until `,` or `)` or `{` or `\n`
    //                         loop {
    //                             match self.peek() {
    //                                 None => break,
    //                                 Some(token) if token.token == Token::Comma => {
    //                                     self.advance();
    //                                     recovered = true;
    //                                     break;
    //                                 }
    //                                 Some(token)
    //                                     if token.token == Token::RParanthesis
    //                                         || token.token == Token::LBracket
    //                                         || token.info.line != line =>
    //                                 {
    //                                     recovered = true;
    //                                     break;
    //                                 }
    //                                 _ => {
    //                                     self.advance();
    //                                 }
    //                             }
    //                         }
    //                     }
    //                 }
    //             }
    //         }
    //     }
    //     components
    // }

    pub fn parse_automaton(&mut self) -> Option<ast::Automaton> {
        None
        // let line = self.get_line();

        // let (name, desc) = match self.expect(Token::Automaton).and(self.parse_ident(line)) {
        //     Err(err) => {
        //         self.errors.push(err);
        //         (None, "".into())
        //     }
        //     Ok((name, desc)) => (Some(name), desc),
        // };
        // let parse_comp = match self.expect(Token::LParanthesis) {
        //     Ok(()) => true,
        //     Err(err) => {
        //         if name.is_some() {
        //             self.errors.push(err);
        //         }
        //         // Error recovery until `(` or  `{`
        //         loop {
        //             match self.peek_token() {
        //                 None => {
        //                     self.errors.push(Error::EOF {
        //                         expected: "'('".into(),
        //                     });
        //                     return None;
        //                 }
        //                 Some(Token::LParanthesis) => {
        //                     self.advance();
        //                     break true;
        //                 }
        //                 Some(Token::LBracket) => {
        //                     break false;
        //                 }
        //                 _ => {
        //                     self.advance();
        //                 }
        //             }
        //         }
        //     }
        // };
        // // Parse component list
        // let components = if parse_comp {
        //     let comps = self.parse_components();
        //     comps
        // } else {
        //     Vec::new()
        // };
        // match self.expect(Token::LBracket) {
        //     Ok(()) => {}
        //     Err(err) => {
        //         // Error recovery until `{`
        //         self.errors.push(err);
        //         loop {
        //             match self.peek_token() {
        //                 None => {
        //                     return None;
        //                 }
        //                 Some(&Token::LBracket) => {
        //                     self.advance();
        //                     break;
        //                 }
        //                 _ => {
        //                     self.advance();
        //                 }
        //             }
        //         }
        //     }
        // }
        // // Parse state list until `}`
        // let states = self.parse_states();
        // match self.expect(Token::RBracket) {
        //     Err(err) => {
        //         self.errors.push(err);
        //         return None;
        //     }
        //     Ok(()) => {}
        // }
        // // Return parsed automaton
        // Some(Automaton {
        //     name: name?,
        //     components,
        //     states,
        //     desc,
        // })
    }

    pub fn parse(mut self) -> ast::Ast {
        let mut automata = Vec::new();
        while self.pos < self.tokens.len() {
            if let Some(automaton) = self.parse_automaton() {
                automata.push(automaton);
            } else {
                break;
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
        ];
        let mut parser = Parser::new(tokens);
        let scope = parser.parse_states();
        assert_eq!(
            scope,
            vec![
                StateScope::Newline,
                StateScope::ErrorTokens {
                    error: Error::Unexpected {
                        expected: "identifier".into(),
                        token: Arrow
                    },
                    location: Some(2),
                    tokens: vec![State, Whitespace, Arrow, Ident("some".into(), "".into())]
                }
            ]
        );

        let tokens = vec![
            State,
            Ident("some".into(), "".into()),
            Arrow,
            Ident("other".into(), "".into()),
        ];
        let mut parser = Parser::new(tokens);
        let scope = parser.parse_states();
        assert_eq!(
            scope,
            vec![StateScope::ErrorTokens {
                error: Error::EOF {
                    expected: "`;`".into()
                },
                location: Some(4),
                tokens: [State, Ident("some".into(), "".into()), Arrow, Ident("other".into(), "".into())].into()
            }]
        );
    }

    //     #[test]
    //     fn test_automaton() {
    //         let tokens = vec![
    //             Automaton,
    //             Ident("main".into(), "entry\npoint".into()),
    //             LParanthesis,
    //             Ident("add".into(), "".into()),
    //             As,
    //             Ident("a1".into(), "".into()),
    //             Comma,
    //             Ident("other_auto".into(), "".into()),
    //             As,
    //             Ident("unused".into(), "".into()),
    //             RParanthesis,
    //             LBracket,
    //             Initial,
    //             State,
    //             Ident("start".into(), "first state".into()),
    //             LBracket,
    //             Symbol('_'),
    //             Slash,
    //             Symbol('0'),
    //             Comma,
    //             Symbol('N'),
    //             Arrow,
    //             Ident("a1".into(), "".into()),
    //             Dot,
    //             Ident("input".into(), "".into()),
    //             Semicolon,
    //             RBracket,
    //             State,
    //             Ident("a1".into(), "final state \n of a \n component".into()),
    //             Dot,
    //             Ident("output".into(), "".into()),
    //             Arrow,
    //             Ident("done".into(), "".into()),
    //             Semicolon,
    //             Accept,
    //             State,
    //             Ident("done".into(), "".into()),
    //             Semicolon,
    //             RBracket,
    //         ];
    //         let mut parser = Parser::new(default_info(tokens));
    //         let a = parser.parse_automaton().unwrap();

    //         assert_eq!(
    //             a,
    //             Automaton {
    //                 name: StringInfo::from("main"),
    //                 components: vec![
    //                     (StringInfo::from("add"), StringInfo::from("a1")),
    //                     (StringInfo::from("other_auto"), StringInfo::from("unused"))
    //                 ],
    //                 states: vec![
    //                     State {
    //                         name: StringInfo::from("start"),
    //                         typ: StateType::State(
    //                             None,
    //                             true,
    //                             vec![Transition {
    //                                 read: '_',
    //                                 write: '0',
    //                                 mov: Move::N,
    //                                 state: (StringInfo::from("input"), Some(StringInfo::from("a1")))
    //                             }]
    //                         ),
    //                         desc: "first state".into()
    //                     },
    //                     State {
    //                         name: StringInfo::from("output"),
    //                         typ: StateType::State(
    //                             Some(StringInfo::from("a1")),
    //                             false,
    //                             vec![Transition {
    //                                 read: '_',
    //                                 write: '_',
    //                                 mov: Move::N,
    //                                 state: (StringInfo::from("done"), None)
    //                             }]
    //                         ),
    //                         desc: "final state \n of a \n component".into()
    //                     },
    //                     State {
    //                         name: StringInfo::from("done"),
    //                         typ: StateType::Accept,
    //                         desc: "".into()
    //                     }
    //                 ],
    //                 desc: "entry\npoint".into()
    //             }
    //         );
    //     }

    //     #[test]
    //     fn test_error_components() {
    //         let tokens = default_info(vec![
    //             Automaton,
    //             Ident("ok".into(), "".into()),
    //             LParanthesis,
    //             Ident("place".into(), "".into()),
    //             Ident("xor".into(), "".into()),
    //             Comma,
    //             Ident("good".into(), "".into()),
    //             As,
    //             Ident("dea?d".into(), "".into()),
    //             Comma,
    //             RParanthesis,
    //             LBracket,
    //             RBracket,
    //         ]);
    //         let mut parser = Parser::new(tokens);
    //         let result = parser.parse_automaton();
    //         assert!(result.is_some());
    //         assert_eq!(
    //             parser.errors,
    //             vec![
    //                 Error::Unexpected {
    //                     token: TokenInfo {
    //                         token: Ident("xor".into(), "".into()),
    //                         info: Info {
    //                             line: 0,
    //                             from: 0,
    //                             to: 0
    //                         }
    //                     },
    //                     expected: "keyword `as`".into()
    //                 },
    //                 Error::Unexpected {
    //                     token: TokenInfo {
    //                         token: RParanthesis,
    //                         info: Info {
    //                             line: 0,
    //                             from: 0,
    //                             to: 0
    //                         }
    //                     },
    //                     expected: "identifier".into()
    //                 }
    //             ]
    //         );
    //     }
}
