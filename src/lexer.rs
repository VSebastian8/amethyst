use crate::{
    info::{Error, Info},
    token::*,
};

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,
    line: u32,
    column: u32,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();

        Self {
            chars: chars.into_iter().peekable(),
            line: 0,
            column: 0,
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        if let Some(&ch) = self.chars.peek() {
            if ch == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }
        self.chars.next()
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Result<Vec<TokenInfo>, Error> {
        let mut tokens = Vec::new();
        while let Some(&ch) = self.chars.peek() {
            if ch.is_whitespace() {
                self.advance();
                continue;
            }
            let mut skip = true;
            let from = self.column;
            let line = self.line;
            let token = match ch {
                '(' => Ok(Token::LParanthesis),
                ')' => Ok(Token::RParanthesis),
                '}' => Ok(Token::RBracket),
                '/' => Ok(Token::Slash),
                ',' => Ok(Token::Comma),
                ';' => Ok(Token::Semicolon),
                '.' => Ok(Token::Dot),
                // special cases
                '{' => {
                    // either bracket or block comment
                    self.advance();
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '-' => {
                                self.advance();
                                self.read_block_comment()?;
                                continue;
                            }
                            _ => {
                                skip = false;
                                Ok(Token::LBracket)
                            }
                        }
                    } else {
                        Err(Error::NotTerminated(
                            "`{`".to_string(),
                            "`}`".to_string(),
                            Info {
                                line,
                                from,
                                to: self.column,
                            },
                        ))
                    }
                }
                '-' => {
                    // either arrow or line comment
                    self.advance();
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '>' => Ok(Token::Arrow),
                            '-' => {
                                self.advance();
                                self.read_line_comment();
                                continue;
                            }
                            _ => {
                                self.advance();
                                Err(Error::Unknown(
                                    '-',
                                    Info {
                                        line,
                                        from,
                                        to: self.column,
                                    },
                                ))
                            }
                        }
                    } else {
                        Err(Error::Unknown(
                            '-',
                            Info {
                                line,
                                from,
                                to: self.column,
                            },
                        ))
                    }
                }
                'a'..='z' => {
                    skip = false;
                    self.read_word()
                }
                'A'..='Z' | '0'..='9' | '_' | '@' | '&' => Ok(Token::Symbol(ch)),
                _ => {
                    self.advance();
                    Err(Error::Unknown(
                        ch,
                        Info {
                            line,
                            from,
                            to: self.column,
                        },
                    ))
                }
            }?;
            if skip {
                self.advance();
            }
            tokens.push(TokenInfo {
                token,
                info: Info {
                    line,
                    from,
                    to: self.column,
                },
            });
        }

        Ok(tokens)
    }

    fn read_line_comment(&mut self) {
        while let Some(c) = self.advance() {
            if c == '\n' {
                break;
            }
        }
    }

    fn read_block_comment(&mut self) -> Result<(), Error> {
        let line = self.line;
        let from = self.column;
        while let Some(c) = self.advance() {
            if c == '-' {
                if let Some(c2) = self.advance() {
                    if c2 == '}' {
                        return Ok(());
                    }
                }
            }
        }
        Err(Error::NotTerminated(
            "block comment".to_string(),
            "`-}`".to_string(),
            Info {
                line,
                from,
                to: from + 2,
            },
        ))
    }

    fn read_word(&mut self) -> Result<Token, Error> {
        let from = self.column;
        let mut word = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() || "(){};,:/-.^@&".contains(c) {
                break;
            } else {
                word.push(c);
                self.advance();
            }
        }
        if let Some(_) =
            word.find(|c: char| !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_')
        {
            return Err(Error::MalformedIdentifier(
                word,
                Info {
                    line: self.line,
                    from: from,
                    to: self.column,
                },
            ));
        }

        Ok(match word.as_str() {
            "automaton" => Token::Automaton,
            "state" => Token::State,
            "initial" => Token::Initial,
            "accept" => Token::Accept,
            "reject" => Token::Reject,
            "as" => Token::As,
            _ => Token::Ident(word),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_whitespace_handling() {
        let mut lexer = Lexer::new("  automaton{  accept state acceptstate }   ");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens.into_iter().map(|t| t.token).collect::<Vec<_>>(),
            vec![
                Automaton,
                LBracket,
                Accept,
                State,
                Ident("acceptstate".to_string()),
                RBracket
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("initial accept state automaton reject");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens.into_iter().map(|t| t.token).collect::<Vec<_>>(),
            vec![Initial, Accept, State, Automaton, Reject,]
        );
    }

    #[test]
    fn test_punctuation() {
        let mut lexer = Lexer::new("(){/ -> ., ;}");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens.into_iter().map(|t| t.token).collect::<Vec<_>>(),
            vec![
                LParanthesis,
                RParanthesis,
                LBracket,
                Slash,
                Arrow,
                Dot,
                Comma,
                Semicolon,
                RBracket
            ]
        );
    }

    #[test]
    fn test_symbols() {
        let mut lexer = Lexer::new("ABZ129_@&");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens.into_iter().map(|t| t.token).collect::<Vec<_>>(),
            vec![
                Symbol('A'),
                Symbol('B'),
                Symbol('Z'),
                Symbol('1'),
                Symbol('2'),
                Symbol('9'),
                Symbol('_'),
                Symbol('@'),
                Symbol('&')
            ]
        );
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("automaton -- This is a line comment\ninitial state first {\n A / B, L -> second_state2; {- This \n - is a - \n multiline comment -}}}");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                TokenInfo {
                    token: Automaton,
                    info: Info {
                        line: 0,
                        from: 0,
                        to: 9
                    }
                },
                TokenInfo {
                    token: Initial,
                    info: Info {
                        line: 1,
                        from: 0,
                        to: 7
                    }
                },
                TokenInfo {
                    token: State,
                    info: Info {
                        line: 1,
                        from: 8,
                        to: 13
                    }
                },
                TokenInfo {
                    token: Ident("first".to_string()),
                    info: Info {
                        line: 1,
                        from: 14,
                        to: 19
                    }
                },
                TokenInfo {
                    token: LBracket,
                    info: Info {
                        line: 1,
                        from: 20,
                        to: 21
                    }
                },
                TokenInfo {
                    token: Symbol('A'),
                    info: Info {
                        line: 2,
                        from: 1,
                        to: 2
                    }
                },
                TokenInfo {
                    token: Slash,
                    info: Info {
                        line: 2,
                        from: 3,
                        to: 4
                    }
                },
                TokenInfo {
                    token: Symbol('B'),
                    info: Info {
                        line: 2,
                        from: 5,
                        to: 6
                    }
                },
                TokenInfo {
                    token: Comma,
                    info: Info {
                        line: 2,
                        from: 6,
                        to: 7
                    }
                },
                TokenInfo {
                    token: Symbol('L'),
                    info: Info {
                        line: 2,
                        from: 8,
                        to: 9
                    }
                },
                TokenInfo {
                    token: Arrow,
                    info: Info {
                        line: 2,
                        from: 10,
                        to: 12
                    }
                },
                TokenInfo {
                    token: Ident("second_state2".to_string()),
                    info: Info {
                        line: 2,
                        from: 13,
                        to: 26
                    }
                },
                TokenInfo {
                    token: Semicolon,
                    info: Info {
                        line: 2,
                        from: 26,
                        to: 27
                    }
                },
                TokenInfo {
                    token: RBracket,
                    info: Info {
                        line: 4,
                        from: 21,
                        to: 22
                    }
                },
                TokenInfo {
                    token: RBracket,
                    info: Info {
                        line: 4,
                        from: 22,
                        to: 23
                    }
                },
            ]
        );
    }

    #[test]
    fn test_invalid_character() {
        let mut lexer = Lexer::new("automaton ?");
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap(),
            Error::Unknown(
                '?',
                Info {
                    line: 0,
                    from: 10,
                    to: 11
                }
            )
        );
    }

    #[test]
    fn test_invalid_identifier() {
        let mut lexer = Lexer::new("state camelCase");
        let result = lexer.tokenize();

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap(),
            Error::MalformedIdentifier(
                "camelCase".to_string(),
                Info {
                    line: 0,
                    from: 6,
                    to: 15
                }
            )
        );
    }
}
