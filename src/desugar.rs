use std::rc::Rc;

use crate::ast;
use crate::cst;
use crate::info::ErrorInfo;
use crate::info::Info;
use crate::info::StringInfo;
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

    fn desugar_line_comment(&mut self, comment: Rc<str>) {
        self.col += comment.len() as u32 + 2;
    }

    fn desugar_block_comment(&mut self, comment: Rc<str>) {
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
            Token::Newline => {
                self.desugar_newline();
                0
            }
            Token::LineComment(comment) => {
                self.desugar_line_comment(comment.clone());
                0
            }
            Token::BlockComment(comment) => {
                self.desugar_block_comment(comment.clone());
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
                let (before, after) = tokens.as_slice().split_at(loc);
                for token in before {
                    self.desugar_token(token);
                }
                let line = self.line;
                let from = self.col;
                if !after.is_empty() {
                    self.desugar_token(&after[0]);
                }
                let to = self.col;
                for token in &after[1..] {
                    self.desugar_token(token);
                }
                Some(Info { line, from, to })
            }
        }
    }

    fn desugar_move(&self, mov: cst::Move) -> ast::Move {
        match mov {
            cst::Move::L => ast::Move::L,
            cst::Move::R => ast::Move::R,
            cst::Move::N => ast::Move::N,
        }
    }

    // Construct AST Transition and skip the relevant columns
    fn desugar_transition(&mut self, transition: cst::Transition) -> ast::Transition {
        // 2 sym + "/,->"
        let parent_from: u32 = self.col + 7 + transition.w[0..6].iter().sum::<u32>();
        let state_name = transition.state.0;
        let parent_to = parent_from
            + transition
                .state
                .1
                .as_ref()
                .map_or(0, |parent| parent.len() as u32);
        let state_from = parent_to + transition.state.1.as_ref().map_or(0, |_| 1);
        let state_to = state_from + state_name.len() as u32;
        // parent.state | state -> (state, Option(parent))
        let state = (
            StringInfo {
                name: state_name,
                info: Info {
                    line: self.line,
                    from: state_from,
                    to: state_to,
                },
            },
            transition.state.1.map(|parent| StringInfo {
                name: parent,
                info: Info {
                    line: self.line,
                    from: parent_from,
                    to: parent_to,
                },
            }),
        );
        // Update column and return
        self.col = state_to + transition.w[6] + 1;
        ast::Transition {
            read: transition.read,
            write: transition.write,
            mov: self.desugar_move(transition.mov),
            state,
        }
    }

    fn desugar_transitions(&mut self, scope: Vec<cst::TransitionScope>) -> Vec<ast::Transition> {
        let mut transitions = vec![];
        for t in scope {
            match t {
                cst::TransitionScope::Transition(transition) => {
                    transitions.push(self.desugar_transition(transition))
                }
                cst::TransitionScope::Whitespace => self.desugar_whitespace(),
                cst::TransitionScope::Newline => self.desugar_newline(),
                cst::TransitionScope::LineComment(comment) => self.desugar_line_comment(comment),
                cst::TransitionScope::BlockComment(comment) => self.desugar_block_comment(comment),
                cst::TransitionScope::ErrorTokens {
                    error,
                    location,
                    tokens,
                } => {
                    let info = self.desugar_tokens(tokens, location);
                    self.errors.push(ErrorInfo { error, info })
                }
            }
        }
        transitions
    }

    pub fn desugar(&mut self, cst: cst::Cst) -> ast::Ast {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::cst;
    use crate::info::{Error, ErrorInfo, Info};
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
            cst::TransitionScope::ErrorTokens {
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
            },
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
}
