use crate::ast::*;
use crate::info::{Error, Info};
use crate::token::*;
pub struct Parser {
    tokens: Vec<TokenInfo>,
    pub errors: Vec<Error>,
    pos: usize,
    lines: Vec<Info>,
}

impl Parser {
    pub fn new(tokens: Vec<TokenInfo>) -> Self {
        let mut lines = Vec::new();
        let mut line = 0;
        let mut last = 0;
        for t in tokens.iter() {
            if t.info.line != line {
                lines.push(Info {
                    line,
                    from: last,
                    to: last + 1,
                });
                line += 1;
                while line < t.info.line {
                    lines.push(Info {
                        line,
                        from: 0,
                        to: 1,
                    });
                    line += 1;
                }
            }
            last = t.info.to;
        }
        lines.push(Info {
            line,
            from: last,
            to: last + 1,
        });
        Parser {
            tokens,
            pos: 0,
            lines,
            errors: Vec::new(),
        }
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

    fn expect(&mut self, expected: &Token) -> Result<(), Error> {
        match self.peek() {
            Some(t) if &t.token == expected => {
                self.advance();
                Ok(())
            }
            Some(token) => Err(Error::Unexpected(token.clone(), expected.to_string())),
            None => Err(Error::EOF(expected.to_string())),
        }
    }

    fn expect_on_line(&mut self, expected: &Token, line: u32) -> Result<(), Error> {
        match self.peek() {
            Some(token) if token.info.line != line => Err(Error::Missing(
                expected.to_string(),
                self.lines[line as usize].clone(),
            )),
            Some(token) if &token.token != expected => {
                Err(Error::Unexpected(token.clone(), expected.to_string()))
            }
            Some(_) => {
                self.advance();
                Ok(())
            }
            None => Err(Error::EOF(expected.to_string())),
        }
    }

    fn get_line(&self) -> u32 {
        match self.peek() {
            Some(token) => token.info.line,
            None => (self.lines.len() - 1) as u32,
        }
    }

    fn get_info(&self) -> Info {
        match self.peek() {
            Some(token) => token.info.clone(),
            None => self.lines.last().unwrap().clone(),
        }
    }

    fn parse_symbol(&mut self, line: u32) -> Result<char, Error> {
        match self.advance() {
            Some(token) if token.info.line != line => Err(Error::Missing(
                "symbol".to_string(),
                self.lines[line as usize].clone(),
            )),
            Some(token) => match &token.token {
                Token::Symbol(ch) => Ok(*ch),
                _ => Err(Error::Unexpected(token.clone(), "symbol".to_string())),
            },
            None => Err(Error::EOF("symbol".to_string())),
        }
    }

    fn parse_move(&mut self, line: u32) -> Result<Move, Error> {
        match self.advance() {
            Some(token) if token.info.line != line => Err(Error::Missing(
                "move symbol".to_string(),
                self.lines[line as usize].clone(),
            )),
            Some(token) => match &token.token {
                Token::Symbol(ch) => match ch {
                    'L' => Ok(Move::L),
                    'R' => Ok(Move::R),
                    'N' => Ok(Move::N),
                    _ => Err(Error::Unexpected(token.clone(), "move symbol".to_string())),
                },
                _ => Err(Error::Unexpected(token.clone(), "move symbol".to_string())),
            },
            None => Err(Error::EOF("symbol".to_string())),
        }
    }

    fn parse_ident(&mut self, line: u32) -> Result<String, Error> {
        match self.peek() {
            Some(token) if token.info.line != line => Err(Error::Missing(
                "identifier".to_string(),
                self.lines[line as usize].clone(),
            )),
            Some(token) => match &token.token {
                Token::Ident(name) => {
                    let res = Ok(name.clone());
                    self.advance();
                    res
                }
                _ => Err(Error::Unexpected(token.clone(), "identifier".to_string())),
            },
            None => Err(Error::EOF("identifier".to_string())),
        }
    }

    fn parse_state_name(&mut self, line: u32) -> Result<(String, Option<String>), Error> {
        let name = self.parse_ident(line)?;
        match self.peek() {
            Some(token) if token.token == Token::Dot && token.info.line == line => {
                self.advance();
                Ok((self.parse_ident(line)?, Some(name)))
            }
            _ => Ok((name, None)),
        }
    }

    fn parse_transition(&mut self) -> Result<Transition, Error> {
        let line = self.get_line();
        let read = self.parse_symbol(line)?;
        self.expect_on_line(&Token::Slash, line)?;
        let write = self.parse_symbol(line)?;
        self.expect_on_line(&Token::Comma, line)?;
        let mov = self.parse_move(line)?;
        self.expect_on_line(&Token::Arrow, line)?;
        let state = self.parse_state_name(line)?;
        self.expect_on_line(&Token::Semicolon, line)?;
        Ok(Transition {
            read,
            write,
            mov,
            state,
        })
    }

    fn parse_transition_recover(&mut self) -> Option<Transition> {
        // Error recovery
        // Consume tokens until `;` or `\n` or `}`
        let line = self.get_line();
        match self.parse_transition() {
            Ok(t) => Some(t),
            Err(err) => {
                self.errors.push(err);
                loop {
                    match self.peek() {
                        Some(token) if token.token == Token::Semicolon => {
                            self.advance();
                            break;
                        }
                        Some(token)
                            if token.token == Token::RBracket || token.info.line != line =>
                        {
                            break;
                        }
                        None => {
                            break;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
                None
            }
        }
    }

    fn parse_transitions(&mut self) -> Vec<Transition> {
        // Parse state transitions until }
        let mut transitions = Vec::new();
        loop {
            match self.peek_token() {
                None | Some(&Token::RBracket) => {
                    break;
                }
                _ => {
                    if let Some(t) = self.parse_transition_recover() {
                        transitions.push(t);
                    }
                }
            }
        }
        transitions
    }

    fn parse_final_state(&mut self, acc: bool) -> Result<State, Error> {
        let line = self.get_line();
        self.advance();
        self.expect_on_line(&Token::State, line)?;
        let state = self.parse_ident(line)?;
        self.expect_on_line(&Token::Semicolon, line)?;
        if acc {
            Ok(State::Accept(state))
        } else {
            Ok(State::Reject(state))
        }
    }

    fn parse_transition_state(&mut self) -> Result<State, Error> {
        let line = self.get_line();
        let init = match self.peek_token() {
            Some(&Token::Initial) => {
                self.advance();
                true
            }
            _ => false,
        };
        self.expect_on_line(&Token::State, line)?;
        let (state, parent) = self.parse_state_name(line)?;
        let mut transitions = Vec::new();
        match self.peek() {
            Some(token) => match token.token {
                Token::Arrow => {
                    self.advance();
                    let new_state = self.parse_state_name(line)?;
                    self.expect(&Token::Semicolon)?;
                    transitions.push(Transition {
                        read: '_',
                        write: '_',
                        mov: Move::N,
                        state: new_state,
                    });
                }
                Token::LBracket => {
                    self.advance();
                    transitions = self.parse_transitions();
                    self.expect(&Token::RBracket)?;
                }
                _ => return Err(Error::Unexpected(token.clone(), "`{` or `->`".to_string())),
            },
            None => return Err(Error::EOF("`{` or `->`".to_string())),
        }

        Ok(State::State(state, parent, init, transitions))
    }

    // Panic mode error recovery for state
    fn state_recover(&mut self) {
        let line = self.get_line();
        loop {
            match self.peek() {
                Some(token) if token.token == Token::Semicolon => {
                    self.advance();
                    break;
                }
                Some(token) if token.token == Token::LBracket => {
                    let info = self.advance().unwrap().info.clone();
                    self.parse_transitions();
                    if self.is_at_end() {
                        self.errors.push(Error::NotTerminated(
                            "`{`".to_string(),
                            "`}`".to_string(),
                            info,
                        ))
                    }
                    self.advance();
                    break;
                }

                Some(token) if token.info.line != line => {
                    break;
                }
                None => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn parse_final_state_recover(&mut self, acc: bool) -> Option<State> {
        match self.parse_final_state(acc) {
            Ok(state) => Some(state),
            Err(e) => {
                self.errors.push(e);
                self.state_recover();
                None
            }
        }
    }

    fn parse_transition_state_recover(&mut self) -> Option<State> {
        match self.parse_transition_state() {
            Ok(state) => Some(state),
            Err(err) => {
                self.errors.push(err.clone());
                match err {
                    Error::Unexpected(_, str) | Error::EOF(str) if str == "`}`" => {
                        // Special case where transitions errors have been added already
                    }
                    _ => self.state_recover(),
                }
                None
            }
        }
    }

    fn parse_state_recover(&mut self) -> Option<State> {
        if let Some(acc) = match self.peek_token() {
            Some(&Token::Accept) => Some(true),
            Some(&Token::Reject) => Some(false),
            _ => None,
        } {
            self.parse_final_state_recover(acc)
        } else {
            self.parse_transition_state_recover()
        }
    }

    fn parse_states(&mut self) -> Vec<State> {
        let mut states = Vec::new();
        loop {
            match self.peek_token() {
                None | Some(&Token::RBracket) => {
                    break;
                }
                _ => {
                    if let Some(s) = self.parse_state_recover() {
                        states.push(s)
                    }
                }
            }
        }
        states
    }

    fn parse_component(&mut self) -> Result<(String, String), Error> {
        let line = self.get_line();
        let path = self.parse_ident(line)?;
        self.expect_on_line(&Token::As, line)?;
        let name = self.parse_ident(line)?;
        Ok((path, name))
    }

    fn parse_components(&mut self) -> Vec<(String, String)> {
        let mut components = Vec::new();
        loop {
            match self.peek_token() {
                None => break,
                Some(&Token::LBracket) => {
                    self.errors
                        .push(Error::Missing("`(`".to_string(), self.get_info()));
                    break;
                }
                Some(&Token::RParanthesis) => {
                    self.advance();
                    break;
                }
                _ => {
                    let sep = if components.len() > 0 {
                        self.expect(&Token::Comma)
                    } else {
                        Ok(())
                    };
                    match sep.and(self.parse_component()) {
                        Ok(c) => components.push(c),
                        Err(err) => {
                            self.errors.push(err);
                            let line = self.get_line();
                            // Error recovery
                            // Advance until `,` or `)` or `{` or `\n`
                            loop {
                                match self.peek() {
                                    None => break,
                                    Some(token)
                                        if token.token == Token::Comma
                                            || token.token == Token::RParanthesis
                                            || token.token == Token::LBracket
                                            || token.info.line != line =>
                                    {
                                        break;
                                    }
                                    _ => {
                                        self.advance();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        components
    }

    pub fn parse_automaton(&mut self) -> Option<Automaton> {
        let line = self.get_line();

        let name = match self.expect(&Token::Automaton).and(self.parse_ident(line)) {
            Err(err) => {
                self.errors.push(err);
                None
            }
            Ok(name) => Some(name),
        };
        let parse_comp = match self.expect(&Token::LParanthesis) {
            Ok(()) => true,
            Err(err) => {
                if name.is_some() {
                    self.errors.push(err);
                }
                // Error recovery until `(` or  `{`
                loop {
                    match self.peek_token() {
                        None => {
                            self.errors.push(Error::EOF("'('".to_string()));
                            return None;
                        }
                        Some(Token::LParanthesis) => {
                            self.advance();
                            break true;
                        }
                        Some(Token::LBracket) => {
                            break false;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
            }
        };
        // Parse component list
        let components = if parse_comp {
            let comps = self.parse_components();
            comps
        } else {
            Vec::new()
        };
        match self.expect(&Token::LBracket) {
            Ok(()) => {}
            Err(err) => {
                // Error recovery until `{`
                self.errors.push(err);
                loop {
                    match self.peek_token() {
                        None => {
                            return None;
                        }
                        Some(&Token::LBracket) => {
                            self.advance();
                            break;
                        }
                        _ => {
                            self.advance();
                        }
                    }
                }
            }
        }
        // Parse state list until `}`
        let states = self.parse_states();
        match self.expect(&Token::RBracket) {
            Err(err) => {
                self.errors.push(err);
                return None;
            }
            Ok(()) => {}
        }
        // Return parsed automaton
        Some(Automaton {
            name: name?,
            components,
            states,
        })
    }

    pub fn parse(&mut self) -> Vec<Automaton> {
        let mut automata = Vec::new();
        while self.pos < self.tokens.len() {
            if let Some(automaton) = self.parse_automaton() {
                automata.push(automaton);
            }
        }
        automata
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
        let s = parser.parse_state_recover().unwrap();

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
        let s = parser.parse_state_recover().unwrap();

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
        let s = parser.parse_state_recover().unwrap();

        assert_eq!(s, State::Accept("done".to_string()));

        let tokens = vec![Reject, State, Ident("over".to_string()), Semicolon];
        let mut parser = Parser::new(default_info(tokens));
        let s = parser.parse_state_recover().unwrap();

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
    fn test_parse_transition_error() {
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
    fn test_parse_state_error() {
        let tokens = default_info(vec![State, Ident("some".to_string()), LBracket]);
        let mut parser = Parser::new(tokens);
        parser.parse_state_recover();
        assert_eq!(parser.errors, vec![Error::EOF("`}`".to_string())]);

        let tokens = default_info(vec![
            State,
            Ident("some".to_string()),
            Arrow,
            Ident("other".to_string()),
        ]);
        let mut parser = Parser::new(tokens);
        parser.parse_state_recover();
        assert_eq!(parser.errors, vec![Error::EOF("`;`".to_string())]);
    }

    #[test]
    fn test_parser_lines() {
        let tokens = vec![
            TokenInfo {
                token: Accept,
                info: Info {
                    line: 0,
                    from: 3,
                    to: 9,
                },
            },
            TokenInfo {
                token: State,
                info: Info {
                    line: 1,
                    from: 15,
                    to: 20,
                },
            },
            TokenInfo {
                token: Ident("whatever".to_string()),
                info: Info {
                    line: 3,
                    from: 0,
                    to: 8,
                },
            },
            TokenInfo {
                token: Semicolon,
                info: Info {
                    line: 3,
                    from: 8,
                    to: 9,
                },
            },
        ];
        let parser = Parser::new(tokens);
        assert_eq!(
            parser.lines,
            vec![
                Info {
                    line: 0,
                    from: 9,
                    to: 10,
                },
                Info {
                    line: 1,
                    from: 20,
                    to: 21,
                },
                Info {
                    line: 2,
                    from: 0,
                    to: 1,
                },
                Info {
                    line: 3,
                    from: 9,
                    to: 10,
                }
            ]
        );
    }
}
