use crate::token::Token;

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();

        Self {
            chars: chars.into_iter().peekable(),
        }
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        while let Some(&ch) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
                continue;
            }
            let token = match ch {
                '(' => Ok(Token::LParanthesis),
                ')' => Ok(Token::RParanthesis),
                '}' => Ok(Token::RParanthesis),
                '/' => Ok(Token::Slash),
                ',' => Ok(Token::Comma),
                ';' => Ok(Token::Semicolon),
                '.' => Ok(Token::Dot),
                // special cases
                '{' => {
                    // either bracket or block comment
                    self.chars.next();
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '-' => {
                                self.chars.next();
                                self.read_block_comment()?;
                                continue;
                            }
                            _ => Ok(Token::LBracket),
                        }
                    } else {
                        Err("Unrecognized - at EOF".to_string())
                    }
                }
                '-' => {
                    // either arrow or line comment
                    self.chars.next();
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '>' => Ok(Token::Arrow),
                            '-' => {
                                self.chars.next();
                                self.read_line_comment();
                                continue;
                            }
                            _ => Err("Unrecognized - ".to_string()),
                        }
                    } else {
                        Err("Unrecognized - at EOF".to_string())
                    }
                }
                'a'..='z' => self.read_word(),
                'A'..='Z' | '0'..='9' | '_' | '@' | '&' => Ok(Token::Symbol(ch)),
                _ => Err(format!("Unexpected character: '{}'", ch)),
            }?;
            if token != Token::LBracket {
                self.chars.next();
            }
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn read_line_comment(&mut self) {
        while let Some(c) = self.chars.next() {
            if c == '\n' {
                break;
            }
        }
    }

    fn read_block_comment(&mut self) -> Result<(), String> {
        while let Some(c) = self.chars.next() {
            if c == '-' {
                if let Some(c2) = self.chars.next() {
                    if c2 == '}' {
                        return Ok(());
                    }
                }
            }
        }
        Err("Unterminated block comment".to_string())
    }

    fn read_word(&mut self) -> Result<Token, String> {
        let mut word = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_uppercase() {
                return Err(format!("Uppercase symbol {} not allowed in identifier", c));
            }
            if c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_' {
                word.push(c);
                self.chars.next();
            } else {
                break;
            }
        }

        Ok(match word.as_str() {
            "automaton" => Token::Automaton,
            "state" => Token::State,
            "initial" => Token::Initial,
            "accept" => Token::Accept,
            "reject" => Token::Reject,
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
        let mut lexer = Lexer::new("  automaton   {  accept state acceptstate }   ");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Automaton,
                LBracket,
                Accept,
                State,
                Ident("acceptstate".to_string()),
                RParanthesis
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("initial accept state automaton reject");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Initial, Accept, State, Automaton, Reject,]);
    }

    #[test]
    fn test_punctuation() {
        let mut lexer = Lexer::new("(){/ -> ., ;}");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                LParanthesis,
                RParanthesis,
                LBracket,
                Slash,
                Arrow,
                Dot,
                Comma,
                Semicolon,
                RParanthesis
            ]
        );
    }

    #[test]
    fn test_symbols() {
        let mut lexer = Lexer::new("ABZ129_@&");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
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
                Automaton,
                Initial,
                State,
                Ident("first".to_string()),
                LBracket,
                Symbol('A'),
                Slash,
                Symbol('B'),
                Comma,
                Symbol('L'),
                Arrow,
                Ident("second_state2".to_string()),
                RParanthesis,
                RParanthesis
            ]
        );
    }

    #[test]
    fn test_invalid_character() {
        let mut lexer = Lexer::new("automaton ?");
        let result = lexer.tokenize();

        assert!(result.is_err());
        if let Err(str) = result {
            assert!(str.contains("Unexpected character"));
        } else {
            panic!("Expected Tokenizer Error");
        }
    }

    #[test]
    fn test_invalid_identifier() {
        let mut lexer = Lexer::new("state camelCase");
        let result = lexer.tokenize();

        assert!(result.is_err());
        if let Err(str) = result {
            assert!(str.contains("Uppercase symbol C not allowed in identifier"));
        } else {
            panic!("Expected Tokenizer Error");
        }
    }
}
