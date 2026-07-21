use crate::ast::*;
use crate::token::*;
pub struct Parser {
    tokens: Vec<TokenInfo>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<TokenInfo>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&TokenInfo> {
        self.tokens.get(self.pos)
    }

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn advance(&mut self) -> Option<&TokenInfo> {
        if !self.is_at_end() {
            let token = &self.tokens[self.pos];
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.peek() {
            Some(t) if &t.token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected {:?}, found end of input", expected)),
        }
    }

    fn parse_symbol(&mut self) -> Result<char, String> {
        match self.advance() {
            Some(t) => match &t.token {
                Token::Symbol(ch) => Ok(*ch),
                other => Err(format!("Expected symbol, found {:?}", other)),
            },
            None => Err(format!("Expected symbol, found EOF")),
        }
    }

    fn parse_move(&mut self) -> Result<Move, String> {
        match self.parse_symbol()? {
            'L' => Ok(Move::L),
            'R' => Ok(Move::R),
            'N' => Ok(Move::N),
            x => Err(format!("Expected move symbol, found {}", x)),
        }
    }

    fn parse_ident(&mut self) -> Result<String, String> {
        match self.advance() {
            Some(t) => match &t.token {
                Token::Ident(name) => Ok(name.clone()),
                other => Err(format!("Expected identifier, found {:?}", other)),
            },
            None => Err(format!("Expected identifier, found EOF")),
        }
    }

    fn parse_state_name(&mut self) -> Result<(String, Option<String>), String> {
        let name = self.parse_ident()?;
        match self.peek_token() {
            Some(&Token::Dot) => {
                self.advance();
                Ok((self.parse_ident()?, Some(name)))
            }
            _ => Ok((name, None)),
        }
    }

    fn parse_transition(&mut self) -> Result<Transition, String> {
        let read = self.parse_symbol()?;
        self.expect(&Token::Slash)?;
        let write = self.parse_symbol()?;
        self.expect(&Token::Comma)?;
        let mov = self.parse_move()?;
        self.expect(&Token::Arrow)?;
        let state = self.parse_state_name()?;
        self.expect(&Token::Semicolon)?;
        Ok(Transition {
            read,
            write,
            mov,
            state,
        })
    }

    fn parse_state(&mut self) -> Result<State, String> {
        if let Some(acc) = match self.peek_token() {
            Some(&Token::Accept) => Some(true),
            Some(&Token::Reject) => Some(false),
            _ => None,
        } {
            self.advance();
            self.expect(&Token::State)?;
            let state = self.parse_ident()?;
            self.expect(&Token::Semicolon)?;
            if acc {
                Ok(State::Accept(state))
            } else {
                Ok(State::Reject(state))
            }
        } else {
            let init = match self.peek_token() {
                Some(&Token::Initial) => {
                    self.advance();
                    true
                }
                _ => false,
            };
            self.expect(&Token::State)?;
            let (state, parent) = self.parse_state_name()?;
            // Parse state transitions until }
            let mut transitions = vec![];
            match self.peek_token() {
                Some(&Token::Arrow) => {
                    self.advance();
                    let new_state = self.parse_state_name()?;
                    self.expect(&Token::Semicolon)?;
                    transitions.push(Transition {
                        read: '_',
                        write: '_',
                        mov: Move::N,
                        state: new_state,
                    });
                }
                Some(&Token::LBracket) => {
                    self.advance();
                    loop {
                        match self.peek_token() {
                            Some(&Token::RBracket) => {
                                self.advance();
                                break;
                            }
                            _ => {
                                let t = self.parse_transition()?;
                                println!("Parsed transition {:?}", t);
                                transitions.push(t);
                            }
                        }
                    }
                }
                _ => return Err("Expected { or  ->".to_string()),
            }
            Ok(State::State(state, parent, init, transitions))
        }
    }

    fn parse_component(&mut self) -> Result<(String, String), String> {
        let path = self.parse_ident()?;
        self.expect(&Token::As)?;
        let name = self.parse_ident()?;
        Ok((path, name))
    }

    pub fn parse_automaton(&mut self) -> Result<Automaton, String> {
        self.expect(&Token::Automaton)?;
        let name = self.parse_ident()?;
        self.expect(&Token::LParanthesis)?;
        // Parse component list until )
        let mut components = vec![];
        loop {
            match self.peek_token() {
                Some(&Token::RParanthesis) => {
                    self.advance();
                    break;
                }
                _ => {
                    if components.len() > 0 {
                        self.expect(&Token::Comma)?;
                    }
                    let c = self.parse_component()?;
                    components.push(c);
                }
            }
        }
        // Parse state list until }
        self.expect(&Token::LBracket)?;
        let mut states = vec![];
        loop {
            match self.peek_token() {
                Some(&Token::RBracket) => {
                    self.advance();
                    break;
                }
                _ => {
                    let s = self.parse_state()?;
                    states.push(s);
                }
            }
        }
        // Return parsed automaton
        Ok(Automaton {
            name,
            components,
            states,
        })
    }

    pub fn parse(&mut self) -> Result<Vec<Automaton>, String> {
        let mut automata = vec![];
        while self.pos < self.tokens.len() {
            let automaton = self.parse_automaton()?;
            automata.push(automaton);
        }
        Ok(automata)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Automaton;
    use crate::ast::State;
    use crate::info::*;
    use Token::*;

    fn default_info(tokens: Vec<Token>) -> Vec<TokenInfo> {
        tokens
            .into_iter()
            .map(|tok| TokenInfo {
                token: tok,
                info: Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            })
            .collect()
    }

    #[test]
    fn test_parse_transition() {
        let tokens = default_info(vec![
            Symbol('A'),
            Slash,
            Symbol('B'),
            Comma,
            Symbol('L'),
            Arrow,
            Ident("s2".to_string()),
            Semicolon,
        ]);
        let mut parser = Parser::new(tokens);
        let t = parser.parse_transition().unwrap();

        assert_eq!(
            t,
            Transition {
                read: 'A',
                write: 'B',
                mov: Move::L,
                state: ("s2".to_string(), None)
            }
        );
    }

    #[test]
    fn test_parse_state() {
        let tokens = vec![
            Initial,
            State,
            Ident("first".to_string()),
            LBracket,
            Symbol('_'),
            Slash,
            Symbol('@'),
            Comma,
            Symbol('R'),
            Arrow,
            Ident("add".to_string()),
            Dot,
            Ident("input".to_string()),
            Semicolon,
            RBracket,
        ];
        let mut parser = Parser::new(default_info(tokens));
        let s = parser.parse_state().unwrap();

        assert_eq!(
            s,
            State::State(
                "first".to_string(),
                None,
                true,
                vec![Transition {
                    read: '_',
                    write: '@',
                    mov: Move::R,
                    state: ("input".to_string(), Some("add".to_string()))
                }]
            )
        );
    }

    #[test]
    fn test_arrow_state() {
        let tokens = default_info(vec![
            State,
            Ident("x".to_string()),
            Dot,
            Ident("some_name".to_string()),
            Arrow,
            Ident("y12".to_string()),
            Dot,
            Ident("some_name2".to_string()),
            Semicolon,
        ]);
        let mut parser = Parser::new(tokens);
        let s = parser.parse_state().unwrap();

        assert_eq!(
            s,
            State::State(
                "some_name".to_string(),
                Some("x".to_string()),
                false,
                vec![Transition {
                    read: '_',
                    write: '_',
                    mov: Move::N,
                    state: ("some_name2".to_string(), Some("y12".to_string()))
                }]
            )
        );
    }

    #[test]
    fn test_final_states() {
        let tokens = vec![Accept, State, Ident("done".to_string()), Semicolon];
        let mut parser = Parser::new(default_info(tokens));
        let s = parser.parse_state().unwrap();

        assert_eq!(s, State::Accept("done".to_string()));

        let tokens = vec![Reject, State, Ident("over".to_string()), Semicolon];
        let mut parser = Parser::new(default_info(tokens));
        let s = parser.parse_state().unwrap();

        assert_eq!(s, State::Reject("over".to_string()));
    }

    #[test]
    fn test_automaton() {
        let tokens = vec![
            Automaton,
            Ident("main".to_string()),
            LParanthesis,
            Ident("add".to_string()),
            As,
            Ident("a1".to_string()),
            Comma,
            Ident("other_auto".to_string()),
            As,
            Ident("unused".to_string()),
            RParanthesis,
            LBracket,
            Initial,
            State,
            Ident("start".to_string()),
            LBracket,
            Symbol('_'),
            Slash,
            Symbol('0'),
            Comma,
            Symbol('N'),
            Arrow,
            Ident("a1".to_string()),
            Dot,
            Ident("input".to_string()),
            Semicolon,
            RBracket,
            State,
            Ident("a1".to_string()),
            Dot,
            Ident("output".to_string()),
            Arrow,
            Ident("done".to_string()),
            Semicolon,
            Accept,
            State,
            Ident("done".to_string()),
            Semicolon,
            RBracket,
        ];
        let mut parser = Parser::new(default_info(tokens));
        let a = parser.parse_automaton().unwrap();

        assert_eq!(
            a,
            Automaton {
                name: "main".to_string(),
                components: vec![
                    ("add".to_string(), "a1".to_string()),
                    ("other_auto".to_string(), "unused".to_string())
                ],
                states: vec![
                    State::State(
                        "start".to_string(),
                        None,
                        true,
                        vec![Transition {
                            read: '_',
                            write: '0',
                            mov: Move::N,
                            state: ("input".to_string(), Some("a1".to_string()))
                        }]
                    ),
                    State::State(
                        "output".to_string(),
                        Some("a1".to_string()),
                        false,
                        vec![Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: ("done".to_string(), None)
                        }]
                    ),
                    State::Accept("done".to_string())
                ]
            }
        );
    }

    #[test]
    fn test_parse_error_transition() {
        let tokens = default_info(vec![
            Symbol('A'),
            Slash,
            Ident("bye".to_string()),
            Comma,
            Symbol('L'),
            Arrow,
            Ident("s2".to_string()),
            Semicolon,
        ]);
        let mut parser = Parser::new(tokens);
        let result = parser.parse_transition();

        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_state() {
        let tokens = default_info(vec![State, Ident("some".to_string()), LBracket]);
        let mut parser = Parser::new(tokens);
        let result = parser.parse_state();
        assert!(result.is_err());

        let tokens = default_info(vec![
            State,
            Ident("some".to_string()),
            Arrow,
            Ident("other".to_string()),
        ]);
        let mut parser = Parser::new(tokens);
        let result = parser.parse_state();
        assert!(result.is_err());
    }
}
