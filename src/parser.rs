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
        while let Some(token) = self.peek() {
            if before.contains(token) {
                break;
            }
            tokens.push(token.clone());
            if until.contains(token) {
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

    // fn parse_transition(&mut self) -> Result<Transition, Error> {
    //     let line = self.get_line();
    //     let read = self.parse_symbol(line)?;
    //     self.expect_on_line(Token::Slash, line)?;
    //     let write = self.parse_symbol(line)?;
    //     self.expect_on_line(Token::Comma, line)?;
    //     let mov = self.parse_move(line)?;
    //     self.expect_on_line(Token::Arrow, line)?;
    //     let (state, parent, _) = self.parse_state_name(line)?;
    //     self.expect_on_line(Token::Semicolon, line)?;
    //     Ok(Transition {
    //         read,
    //         write,
    //         mov,
    //         state: (state, parent),
    //     })
    // }

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
                    location: None,
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
                                &[Token::Newline, Token::Semicolon],
                                &[Token::RBracket],
                            ),
                        },
                    }
                }
            },
        }
    }

    // fn parse_transition_recover(&mut self) -> Option<Transition> {
    //     // Error recovery
    //     // Consume tokens until `;` or `\n` or `}`
    //     let line = self.get_line();
    //     match self.parse_transition() {
    //         Ok(t) => Some(t),
    //         Err(err) => {
    //             self.errors.push(err);
    //             loop {
    //                 match self.peek() {
    //                     Some(token) if token.token == Token::Semicolon => {
    //                         self.advance();
    //                         break;
    //                     }
    //                     Some(token)
    //                         if token.token == Token::RBracket || token.info.line != line =>
    //                     {
    //                         break;
    //                     }
    //                     None => {
    //                         break;
    //                     }
    //                     _ => {
    //                         self.advance();
    //                     }
    //                 }
    //             }
    //             None
    //         }
    //     }
    // }

    // fn parse_transitions(&mut self) -> Vec<Transition> {
    //     // Parse state transitions until }
    //     let mut transitions = Vec::new();
    //     loop {
    //         match self.peek_token() {
    //             None | Some(&Token::RBracket) => {
    //                 break;
    //             }
    //             _ => {
    //                 if let Some(t) = self.parse_transition_recover() {
    //                     transitions.push(t);
    //                 }
    //             }
    //         }
    //     }
    //     transitions
    // }

    // fn parse_final_state(&mut self, acc: bool) -> Result<State, Error> {
    //     let line = self.get_line();
    //     self.advance();
    //     self.expect_on_line(Token::State, line)?;
    //     let (state, desc) = self.parse_ident(line)?;
    //     self.expect_on_line(Token::Semicolon, line)?;
    //     if acc {
    //         Ok(State {
    //             name: state,
    //             typ: StateType::Accept,
    //             desc,
    //         })
    //     } else {
    //         Ok(State {
    //             name: state,
    //             typ: StateType::Reject,
    //             desc,
    //         })
    //     }
    // }

    // fn parse_transition_state(&mut self) -> Result<State, Error> {
    //     let line = self.get_line();
    //     let init = match self.peek_token() {
    //         Some(&Token::Initial) => {
    //             self.advance();
    //             true
    //         }
    //         _ => false,
    //     };
    //     self.expect_on_line(Token::State, line)?;
    //     let (state, parent, desc) = self.parse_state_name(line)?;
    //     let mut transitions = Vec::new();
    //     match self.peek() {
    //         Some(token) => match token.token {
    //             Token::Arrow => {
    //                 self.advance();
    //                 let (new_state, new_parent, _) = self.parse_state_name(line)?;
    //                 self.expect(Token::Semicolon)?;
    //                 transitions.push(Transition {
    //                     read: '_',
    //                     write: '_',
    //                     mov: Move::N,
    //                     state: (new_state, new_parent),
    //                 });
    //             }
    //             Token::LBracket => {
    //                 self.advance();
    //                 transitions = self.parse_transitions();
    //                 self.expect(Token::RBracket)?;
    //             }
    //             _ => {
    //                 return Err(Error::Unexpected {
    //                     token: token.clone(),
    //                     expected: "`{` or `->`".into(),
    //                 })
    //             }
    //         },
    //         None => {
    //             return Err(Error::EOF {
    //                 expected: "`{` or `->`".into(),
    //             })
    //         }
    //     }

    //     Ok(State {
    //         name: state,
    //         typ: StateType::State(parent, init, transitions),
    //         desc,
    //     })
    // }

    // // Panic mode error recovery for state
    // fn state_recover(&mut self) {
    //     let line = self.get_line();
    //     loop {
    //         match self.peek() {
    //             Some(token) if token.token == Token::Semicolon => {
    //                 self.advance();
    //                 break;
    //             }
    //             Some(token) if token.token == Token::LBracket => {
    //                 let info = self.advance().unwrap().info.clone();
    //                 self.parse_transitions();
    //                 if self.is_at_end() {
    //                     self.errors.push(Error::NotTerminated {
    //                         start: "`{`".into(),
    //                         end: "`}`".into(),
    //                         info,
    //                     })
    //                 }
    //                 self.advance();
    //                 break;
    //             }

    //             Some(token) if token.info.line != line => {
    //                 break;
    //             }
    //             None => {
    //                 break;
    //             }
    //             _ => {
    //                 self.advance();
    //             }
    //         }
    //     }
    // }

    // fn parse_final_state_recover(&mut self, acc: bool) -> Option<State> {
    //     match self.parse_final_state(acc) {
    //         Ok(state) => Some(state),
    //         Err(e) => {
    //             self.errors.push(e);
    //             self.state_recover();
    //             None
    //         }
    //     }
    // }

    // fn parse_transition_state_recover(&mut self) -> Option<State> {
    //     match self.parse_transition_state() {
    //         Ok(state) => Some(state),
    //         Err(err) => {
    //             self.errors.push(err.clone());
    //             match err {
    //                 Error::Unexpected { expected, .. } | Error::EOF { expected }
    //                     if expected.as_ref() == "`}`" =>
    //                 {
    //                     // Special case where transitions errors have been added already
    //                 }
    //                 _ => self.state_recover(),
    //             }
    //             None
    //         }
    //     }
    // }

    // fn parse_state_recover(&mut self) -> Option<State> {
    //     if let Some(acc) = match self.peek_token() {
    //         Some(&Token::Accept) => Some(true),
    //         Some(&Token::Reject) => Some(false),
    //         _ => None,
    //     } {
    //         self.parse_final_state_recover(acc)
    //     } else {
    //         self.parse_transition_state_recover()
    //     }
    // }

    // fn parse_states(&mut self) -> Vec<State> {
    //     let mut states = Vec::new();
    //     loop {
    //         match self.peek_token() {
    //             None | Some(&Token::RBracket) => {
    //                 break;
    //             }
    //             _ => {
    //                 if let Some(s) = self.parse_state_recover() {
    //                     states.push(s)
    //                 }
    //             }
    //         }
    //     }
    //     states
    // }

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

    //     #[test]
    //     fn test_parse_state() {
    //         let tokens = vec![
    //             Initial,
    //             State,
    //             Ident("first".into(), "this is the initial state".into()),
    //             LBracket,
    //             Symbol('_'),
    //             Slash,
    //             Symbol('@'),
    //             Comma,
    //             Symbol('R'),
    //             Arrow,
    //             Ident("add".into(), "".into()),
    //             Dot,
    //             Ident("input".into(), "".into()),
    //             Semicolon,
    //             RBracket,
    //         ];
    //         let mut parser = Parser::new(default_info(tokens));
    //         let s = parser.parse_state_recover().unwrap();

    //         assert_eq!(
    //             s,
    //             State {
    //                 name: StringInfo::from("first"),
    //                 typ: StateType::State(
    //                     None,
    //                     true,
    //                     vec![Transition {
    //                         read: '_',
    //                         write: '@',
    //                         mov: Move::R,
    //                         state: (StringInfo::from("input"), Some(StringInfo::from("add")))
    //                     }]
    //                 ),
    //                 desc: "this is the initial state".into()
    //             }
    //         );
    //     }

    //     #[test]
    //     fn test_arrow_state() {
    //         let tokens = default_info(vec![
    //             State,
    //             Ident("x".into(), "could be y".into()),
    //             Dot,
    //             Ident("some_name".into(), "".into()),
    //             Arrow,
    //             Ident("y12".into(), "".into()),
    //             Dot,
    //             Ident("some_name2".into(), "".into()),
    //             Semicolon,
    //         ]);
    //         let mut parser = Parser::new(tokens);
    //         let s = parser.parse_state_recover().unwrap();

    //         assert_eq!(
    //             s,
    //             State {
    //                 name: StringInfo::from("some_name"),
    //                 typ: StateType::State(
    //                     Some(StringInfo::from("x")),
    //                     false,
    //                     vec![Transition {
    //                         read: '_',
    //                         write: '_',
    //                         mov: Move::N,
    //                         state: (
    //                             StringInfo::from("some_name2"),
    //                             Some(StringInfo::from("y12"))
    //                         )
    //                     }]
    //                 ),
    //                 desc: "could be y".into()
    //             }
    //         );
    //     }

    //     #[test]
    //     fn test_final_states() {
    //         let tokens = vec![
    //             Accept,
    //             State,
    //             Ident("done".into(), "final state".into()),
    //             Semicolon,
    //         ];
    //         let mut parser = Parser::new(default_info(tokens));
    //         let s = parser.parse_state_recover().unwrap();

    //         assert_eq!(
    //             s,
    //             State {
    //                 name: StringInfo::from("done"),
    //                 typ: StateType::Accept,
    //                 desc: "final state".into()
    //             }
    //         );

    //         let tokens = vec![Reject, State, Ident("over".into(), "".into()), Semicolon];
    //         let mut parser = Parser::new(default_info(tokens));
    //         let s = parser.parse_state_recover().unwrap();

    //         assert_eq!(
    //             s,
    //             State {
    //                 name: StringInfo::from("over"),
    //                 typ: StateType::Reject,
    //                 desc: "".into()
    //             }
    //         );
    //     }

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
    //     fn test_parse_state_error() {
    //         let tokens = default_info(vec![State, Ident("some".into(), "".into()), LBracket]);
    //         let mut parser = Parser::new(tokens);
    //         parser.parse_state_recover();
    //         assert_eq!(
    //             parser.errors,
    //             vec![Error::EOF {
    //                 expected: "`}`".into()
    //             }]
    //         );

    //         let tokens = default_info(vec![
    //             State,
    //             Ident("some".into(), "".into()),
    //             Arrow,
    //             Ident("other".into(), "".into()),
    //         ]);
    //         let mut parser = Parser::new(tokens);
    //         parser.parse_state_recover();
    //         assert_eq!(
    //             parser.errors,
    //             vec![Error::EOF {
    //                 expected: "`;`".into()
    //             }]
    //         );
    //     }

    //     #[test]
    //     fn test_parser_lines() {
    //         let tokens = vec![
    //             TokenInfo {
    //                 token: Accept,
    //                 info: Info {
    //                     line: 0,
    //                     from: 3,
    //                     to: 9,
    //                 },
    //             },
    //             TokenInfo {
    //                 token: State,
    //                 info: Info {
    //                     line: 1,
    //                     from: 15,
    //                     to: 20,
    //                 },
    //             },
    //             TokenInfo {
    //                 token: Ident("whatever".into(), "".into()),
    //                 info: Info {
    //                     line: 3,
    //                     from: 0,
    //                     to: 8,
    //                 },
    //             },
    //             TokenInfo {
    //                 token: Semicolon,
    //                 info: Info {
    //                     line: 3,
    //                     from: 8,
    //                     to: 9,
    //                 },
    //             },
    //         ];
    //         let parser = Parser::new(tokens);
    //         assert_eq!(
    //             parser.lines,
    //             vec![
    //                 Info {
    //                     line: 0,
    //                     from: 9,
    //                     to: 10,
    //                 },
    //                 Info {
    //                     line: 1,
    //                     from: 20,
    //                     to: 21,
    //                 },
    //                 Info {
    //                     line: 2,
    //                     from: 0,
    //                     to: 1,
    //                 },
    //                 Info {
    //                     line: 3,
    //                     from: 9,
    //                     to: 10,
    //                 }
    //             ]
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
