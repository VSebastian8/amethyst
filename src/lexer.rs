use std::mem;

use crate::token::*;

pub struct Lexer {
    chars: std::iter::Peekable<std::vec::IntoIter<char>>,
    description: String,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let chars: Vec<char> = input.chars().collect();

        Self {
            chars: chars.into_iter().peekable(),
            description: String::new(),
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Tokenize the entire input
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(&ch) = self.chars.peek() {
            let mut skip = true;
            let token = match ch {
                ' ' => Token::Whitespace,
                '\n' => Token::Newline,
                '(' => Token::LParanthesis,
                ')' => Token::RParanthesis,
                '}' => {
                    self.description = String::new();
                    Token::RBracket
                }
                '/' => Token::Slash,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '.' => Token::Dot,
                // special cases
                '{' => {
                    // either bracket or block comment
                    self.advance();
                    skip = false;
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '-' => {
                                self.advance();
                                self.read_block_comment()
                            }
                            _ => Token::LBracket,
                        }
                    } else {
                        Token::LBracket
                    }
                }
                '-' => {
                    // either arrow or line comment
                    self.advance();
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '>' => Token::Arrow,
                            '-' => {
                                self.advance();
                                skip = false;
                                self.read_line_comment()
                            }
                            _ => {
                                skip = false;
                                Token::Unknown(ch)
                            }
                        }
                    } else {
                        Token::Unknown(ch)
                    }
                }
                'a'..='z' => {
                    skip = false;
                    self.read_word()
                }
                'A'..='Z' | '0'..='9' | '_' | '@' | '&' => Token::Symbol(ch),
                _ => Token::Unknown(ch),
            };
            if skip {
                self.advance();
            }
            tokens.push(token);
        }
        // Return the tokens
        tokens
    }

    fn read_line_comment(&mut self) -> Token {
        let mut comment = String::new();
        while let Some(c) = self.advance() {
            comment.push(c);
            if c == '\n' {
                break;
            }
        }
        self.description.push_str(&comment);
        Token::LineComment(comment.into())
    }

    fn read_block_comment(&mut self) -> Token {
        let mut comment = String::new();
        while let Some(c) = self.advance() {
            if c == '-' {
                if let Some(c2) = self.advance() {
                    if c2 == '}' {
                        self.description.push_str(&comment);
                        break;
                    } else {
                        comment.push(c);
                        comment.push(c2);
                    }
                }
            } else {
                comment.push(c);
            }
        }
        Token::BlockComment(comment.into())
    }

    fn read_word(&mut self) -> Token {
        let mut word = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() || "(){};,:/-.^@&".contains(c) {
                break;
            } else {
                word.push(c);
                self.advance();
            }
        }

        match word.as_str() {
            "automaton" => Token::Automaton,
            "state" => Token::State,
            "initial" => Token::Initial,
            "accept" => Token::Accept,
            "reject" => Token::Reject,
            "as" => Token::As,
            _ => {
                let desc = mem::replace(&mut self.description, String::new());
                Token::Ident(word.into(), desc.trim().into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("");
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_whitespace_handling() {
        let mut lexer = Lexer::new("  automaton{  accept state acceptstate }   ");
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            vec![
                Whitespace,
                Whitespace,
                Automaton,
                LBracket,
                Whitespace,
                Whitespace,
                Accept,
                Whitespace,
                State,
                Whitespace,
                Ident("acceptstate".into(), "".into()),
                Whitespace,
                RBracket,
                Whitespace,
                Whitespace,
                Whitespace
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("initial accept state automaton reject");
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            vec![
                Initial, Whitespace, Accept, Whitespace, State, Whitespace, Automaton, Whitespace,
                Reject
            ]
        );
    }

    #[test]
    fn test_punctuation() {
        let mut lexer = Lexer::new("(){/ -> ., ;}");
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            vec![
                LParanthesis,
                RParanthesis,
                LBracket,
                Slash,
                Whitespace,
                Arrow,
                Whitespace,
                Dot,
                Comma,
                Whitespace,
                Semicolon,
                RBracket
            ]
        );
    }

    #[test]
    fn test_symbols() {
        let mut lexer = Lexer::new("ABZ129_@&");
        let tokens = lexer.tokenize();
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
        let tokens = lexer.tokenize();
        assert_eq!(
            tokens,
            vec![
                Automaton,
                Whitespace,
                LineComment(" This is a line comment\n".into()),
                Initial,
                Whitespace,
                State,
                Whitespace,
                Ident("first".into(), "This is a line comment".into()),
                Whitespace,
                LBracket,
                Newline,
                Whitespace,
                Symbol('A'),
                Whitespace,
                Slash,
                Whitespace,
                Symbol('B'),
                Comma,
                Whitespace,
                Symbol('L'),
                Whitespace,
                Arrow,
                Whitespace,
                Ident("second_state2".into(), "".into()),
                Semicolon,
                Whitespace,
                BlockComment(" This \n - is a - \n multiline comment ".into()),
                RBracket,
                RBracket
            ]
        );
    }

    #[test]
    fn test_descriptions() {
        let mut lexer = Lexer::new("-- This turing machine \n -- is pretty neat \n automaton add(a as b) { \n--other ignored comment\n } {- this \n state -} -- is cool \n state ups {- some\nthing - } ");
        let tokens = lexer.tokenize();

        assert_eq!(
            tokens,
            vec![
                LineComment(" This turing machine \n".into()),
                Whitespace,
                LineComment(" is pretty neat \n".into()),
                Whitespace,
                Automaton,
                Whitespace,
                Ident("add".into(), "This turing machine \n is pretty neat".into()),
                LParanthesis,
                Ident("a".into(), "".into()),
                Whitespace,
                As,
                Whitespace,
                Ident("b".into(), "".into()),
                RParanthesis,
                Whitespace,
                LBracket,
                Whitespace,
                Newline,
                LineComment("other ignored comment\n".into()),
                Whitespace,
                RBracket,
                Whitespace,
                BlockComment(" this \n state ".into()),
                Whitespace,
                LineComment(" is cool \n".into()),
                Whitespace,
                State,
                Whitespace,
                Ident("ups".into(), "this \n state  is cool".into()),
                Whitespace,
                BlockComment(" some\nthing - } ".into())
            ]
        );
    }
}
