#![allow(dead_code)]

use crate::syntax::{
    AutomatonType, Machine, MacroType, Move, Program, State, StateType, Transition,
};

const ALLOWED_NAME_SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyz0123456789_.";
const ALLOWED_TAPE_SYMBOLS: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*[]-+=/?_:";
const BREAK_SYMBOLS: &str = "(){};=, \n\"\'";

// Check if a character is valid for a name
fn allowed_name(x: &char) -> bool {
    ALLOWED_NAME_SYMBOLS.contains(*x)
}

// Check if a character is valid for a name
fn allowed_tape(x: &char) -> bool {
    ALLOWED_TAPE_SYMBOLS.contains(*x)
}

// Check if a character is valid for a literal
fn literal(x: &char) -> bool {
    !BREAK_SYMBOLS.contains(*x)
}

// Content left to parse, with a header to the current position
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Leftover<'a> {
    pub input: &'a str,
    pub row: u32,
    pub col: u32,
}

// mutating and non-mutating lookahead
impl<'a> Leftover<'a> {
    // mutating lookahead
    fn advance(&mut self) -> Option<char> {
        let mut chars = self.input.chars();
        match chars.next() {
            Some(c) => {
                self.input = chars.as_str();
                if c != '\n' {
                    self.col += 1;
                } else {
                    self.row += 1;
                    self.col = 0;
                }
                Some(c)
            }
            None => None,
        }
    }
    // non mutating lookahead
    fn look(&self) -> Option<char> {
        match self.input.chars().nth(0) {
            Some(c) => Some(c),
            None => None,
        }
    }
    // mutating literal lookahead
    fn advance_literal(&mut self) -> usize {
        let index = self
            .input
            .char_indices()
            .find(|(_, c)| !literal(c))
            .map(|(i, _)| i)
            .unwrap_or(self.input.len());
        self.col += index as u32;
        index
    }
    // non-mutating literal lookahead
    fn look_literal(&self) -> &str {
        let index = self
            .input
            .char_indices()
            .find(|(_, c)| !literal(c))
            .map(|(i, _)| i)
            .unwrap_or(self.input.len());

        &self.input[..index]
    }
}

// Helper trait for parsers that return vectors
trait IsEmpty {
    fn is_empty(&self) -> bool;
}

impl<T> IsEmpty for Vec<T> {
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl IsEmpty for &str {
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Encapsulated function for parser combinators
struct Parser<'a, T> {
    parse: Box<dyn Fn(Leftover<'a>) -> Option<(Leftover<'a>, Result<T, String>)> + 'a>,
}

// Implementation functor, applicative, and alternative instances + condition, not_null, many, and some
impl<'a, T: 'a> Parser<'a, T> {
    // (<$>) operator
    fn fmap<B: 'a, F>(self, f: F) -> Parser<'a, B>
    where
        F: Fn(T) -> B + 'a,
    {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover, res)) => Some((
                    leftover,
                    match res {
                        Err(msg) => Err(msg),
                        Ok(x) => Ok(f(x)),
                    },
                )),
            }),
        }
    }

    // (<*>) operator
    fn ap<A: 'a, B: 'a>(self, p: Parser<'a, A>) -> Parser<'a, B>
    where
        T: Fn(A) -> B,
    {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover1, res1)) => match res1 {
                    Err(msg) => Some((leftover1, Err(msg))),
                    Ok(f) => match (p.parse)(leftover1) {
                        None => None,
                        Some((leftover2, res2)) => Some((
                            leftover2,
                            match res2 {
                                Err(msg) => Err(msg),
                                Ok(x) => Ok(f(x)),
                            },
                        )),
                    },
                },
            }),
        }
    }

    // (<*) operator
    fn left<B: 'a>(self, p: Parser<'a, B>) -> Parser<'a, T> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover1, res1)) => match res1 {
                    Err(msg) => Some((leftover1, Err(msg))),
                    Ok(x) => match (p.parse)(leftover1) {
                        None => None,
                        Some((leftover2, res2)) => match res2 {
                            Err(msg) => Some((leftover2, Err(msg))),
                            Ok(_) => Some((leftover2, Ok(x))),
                        },
                    },
                },
            }),
        }
    }

    // (*>) operator
    fn right<B: 'a>(self, p: Parser<'a, B>) -> Parser<'a, B> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover1, res1)) => match res1 {
                    Err(msg) => Some((leftover1, Err(msg))),
                    Ok(_) => match (p.parse)(leftover1) {
                        None => None,
                        Some((leftover2, res2)) => match res2 {
                            Err(msg) => Some((leftover2, Err(msg))),
                            Ok(y) => Some((leftover2, Ok(y))),
                        },
                    },
                },
            }),
        }
    }

    // (<|>) operator
    fn or(self, p2: Parser<'a, T>) -> Parser<'a, T> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                Some(output) => Some(output),
                None => (p2.parse)(input),
            }),
        }
    }

    // check if the parsed content passes a certain condition
    fn condition<C>(self, cond: C) -> Parser<'a, T>
    where
        C: Fn(&T) -> bool + 'a,
    {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover, res)) => match res {
                    Ok(results) => {
                        if cond(&results) {
                            Some((leftover, Ok(results)))
                        } else {
                            None
                        }
                    }
                    err => Some((leftover, err)),
                },
            }),
        }
    }

    // condition parser with custom error
    fn condition_e<C>(self, cond: C, msg: &'a str) -> Parser<'a, T>
    where
        C: Fn(&T) -> bool + 'a,
    {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                None => None,
                Some((leftover, res)) => Some((
                    leftover,
                    match res {
                        Ok(results) => {
                            if cond(&results) {
                                Ok(results)
                            } else {
                                Err(msg.to_string())
                            }
                        }
                        err => err,
                    },
                )),
            }),
        }
    }

    // check that the vector has at least one element
    fn not_null(self) -> Parser<'a, T>
    where
        T: IsEmpty,
    {
        self.condition(|v| !v.is_empty())
    }

    // not_null with custom error
    fn not_null_e(self, msg: &'a str) -> Parser<'a, T>
    where
        T: IsEmpty,
    {
        self.condition_e(|v| !v.is_empty(), msg)
    }

    // 0 or more chained parsers of the same type
    fn many(self) -> Parser<'a, Vec<T>> {
        Parser {
            parse: Box::new(move |input| {
                let mut results = Vec::new();
                let mut current_input = input;
                while let Some((next_input, res)) = (self.parse)(current_input) {
                    match res {
                        Err(msg) => return Some((next_input, Err(msg))),
                        Ok(x) => {
                            results.push(x);
                            current_input = next_input;
                        }
                    }
                }
                Some((current_input, Ok(results)))
            }),
        }
    }

    // 1 or more chained parsers of the same type
    fn some(self) -> Parser<'a, Vec<T>> {
        self.many().not_null()
    }

    // chaining parsers that pass a condition
    fn span<C>(self, cond: C) -> Parser<'a, Vec<T>>
    where
        C: Fn(&T) -> bool + 'a,
    {
        self.condition(cond).many()
    }

    // span parser with custom error
    fn span_e<C>(self, cond: C, msg: &'a str) -> Parser<'a, Vec<T>>
    where
        C: Fn(&T) -> bool + 'a,
    {
        self.condition_e(cond, msg).many()
    }

    // raise an error if the parser fails
    fn raise(self, msg: &'a str) -> Parser<'a, T> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                Some(result) => Some(result),
                None => Some((input, Err(msg.to_string()))),
            }),
        }
    }
    // raise an error and append the current char
    fn raise_look(self, msg: &'a str) -> Parser<'a, T> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                Some(result) => Some(result),
                None => Some((
                    input,
                    Err(format!(
                        "{}{}",
                        msg,
                        match input.look() {
                            None => "no more input".to_string(),
                            Some(c) => c.to_string(),
                        }
                    )),
                )),
            }),
        }
    }
    // raise an error and append the literal to the end
    fn raise_literal(self, prefix: &'a str, msg: &'a str, postfix: &'a str) -> Parser<'a, T> {
        Parser {
            parse: Box::new(move |input| match (self.parse)(input) {
                Some(result) => Some(result),
                None => Some((
                    input,
                    Err(format!(
                        "{}{}{}{}",
                        prefix,
                        msg,
                        postfix,
                        input.look_literal()
                    )),
                )),
            }),
        }
    }
}

// parse a single character
fn any_char_p<'a>() -> Parser<'a, char> {
    Parser {
        parse: Box::new(move |mut input| {
            let current = input.advance();
            match current {
                None => None,
                Some(c) => Some((input, Ok(c))),
            }
        }),
    }
}

// fail if the character differs
fn char_p<'a>(x: char) -> Parser<'a, char> {
    Parser {
        parse: Box::new(move |mut input| {
            let current = input.advance();
            match current {
                None => None,
                Some(c) if c != x => None,
                _ => Some((input, Ok(x))),
            }
        }),
    }
}

// raise an error if the character differs
fn char_pe<'a>(x: char) -> Parser<'a, char> {
    Parser {
        parse: Box::new(move |mut input| {
            let current = input.advance();
            match current {
                None => Some((input, Err(format!("Expected {} found no more input", x)))),
                Some(c) if c != x => Some((input, Err(format!("Expected {} found {}", x, c)))),
                _ => Some((input, Ok(x))),
            }
        }),
    }
}

// make sure the current character is not x without consuming it
fn not_char_p<'a>(x: char) -> Parser<'a, ()> {
    Parser {
        parse: Box::new(move |input| match input.look() {
            Some(c) if c == x => None,
            _ => Some((input, Ok(()))),
        }),
    }
}

// parser for a single move symbol
fn move_pe<'a>() -> Parser<'a, Move> {
    (char_p('L').fmap(|_| Move::Left))
        .or(char_p('R').fmap(|_| Move::Right))
        .or(char_p('N').fmap(|_| Move::Neutral))
        .raise_look("Expected move symbol found ")
}

// whitespace parser, never files
fn ws0<'a>() -> Parser<'a, &'a str> {
    char_p(' ').many().fmap(|_| "")
}

// whitespace parser + new lines, never fails
fn ws1<'a>() -> Parser<'a, &'a str> {
    (char_p(' ').or(char_p('\n'))).many().fmap(|_| "")
}

// parser for a single tape symbol
fn symbol_p<'a>() -> Parser<'a, char> {
    any_char_p().condition(allowed_tape)
}

// symbol parser that raises an error
fn symbol_pe<'a>() -> Parser<'a, char> {
    symbol_p().raise_look("Expected tape symbol found ")
}

// parse any literal (string that has no break symbols)
fn literal_p<'a>() -> Parser<'a, &'a str> {
    Parser {
        parse: Box::new(|mut input| {
            let index = input.advance_literal();
            let lit = &input.input[..index];
            input.input = &input.input[index..];
            Some((input, Ok(lit)))
        }),
    }
}

// literal with allowed name characters
fn word_pe<'a>() -> Parser<'a, &'a str> {
    literal_p()
        .condition(|s| s.chars().all(|c| allowed_name(&c)))
        .raise_literal("", "Forbidden symbol in word ", "")
}

// literal with allowed tape characters
fn tape_pe<'a>() -> Parser<'a, &'a str> {
    literal_p()
        .condition(|s| s.chars().all(|c| allowed_tape(&c)))
        .raise_literal("", "Forbidden symbol in tape sequence ", "")
}

// parse a specific string, using &str for efficiency
fn string_p<'a>(s: &'a str) -> Parser<'a, &'a str> {
    Parser {
        parse: Box::new(move |mut input| {
            let mut chars = s.chars();
            while let Some(expected) = chars.next() {
                match input.advance() {
                    Some(c) if c == expected => {}
                    _ => return None,
                }
            }
            Some((input, Ok(s)))
        }),
    }
}

// string parser that raises an error
fn string_pe<'a>(s: &'a str) -> Parser<'a, &'a str> {
    string_p(s).raise_literal("Expected ", s, " found ")
}

// flipped arguments for fmap
fn fmake<'a, A: 'a, B: 'a, F>(f: F) -> Parser<'a, F>
where
    F: Fn(A) -> B + 'a,
    F: Clone,
{
    Parser {
        parse: Box::new(move |input| Some((input, Ok(f.clone())))),
    }
}

// parser for transition
fn transition_pe<'a>() -> Parser<'a, Transition> {
    fmake(|read_sym| {
        move |write_sym| move |move_sym| move |new_state| (read_sym, write_sym, move_sym, new_state)
    })
    .ap(ws1().right(symbol_pe()).left(ws0()).left(char_pe('/')))
    .ap(ws0().right(symbol_pe()).left(ws0()).left(char_pe(',')))
    .ap(ws0().right(move_pe()).left(ws1()).left(string_pe("->")))
    .ap(ws0().right(
        word_pe()
            .not_null_e("Expected new state")
            .left(ws0())
            .left(char_pe(';')),
    ))
    .fmap(
        |(read_symbol, write_symbol, move_symbol, new_state)| Transition {
            read_symbol,
            write_symbol,
            move_symbol,
            new_state: new_state.to_string(),
        },
    )
}

// whitespace parser that requires at least one blank character
fn ws2<'a>() -> Parser<'a, &'a str> {
    (char_p(' ').or(char_p('\n')))
        .some()
        .fmap(|_| "")
        .raise("Expected space")
}

// make sure a character is not followed by another, consume only the first char
fn not_followed<'a>(x: char, y: char) -> Parser<'a, char> {
    Parser {
        parse: Box::new(move |mut input| {
            let current = input.advance();
            match current {
                None => None,
                Some(c) if c != x => Some((input, Ok(x))),
                _ => match input.look() {
                    None => Some((input, Ok(x))),
                    Some(z) if z != y => Some((input, Ok(x))),
                    _ => None,
                },
            }
        }),
    }
}

// parser for line and block comments
fn comment_pe<'a>() -> Parser<'a, &'a str> {
    string_p("--")
        .right(any_char_p().span(|x| *x != '\n'))
        .or(string_p("{-")
            .right(not_followed('-', '}').many())
            .left(string_pe("-}")))
        .fmap(|_| "")
}

// whitespace + comments parser
fn ws3<'a>() -> Parser<'a, &'a str> {
    ws1().left(comment_pe().right(ws1()).many())
}

// parser for state
fn state_pe<'a>() -> Parser<'a, StateType> {
    let final_p = fmake(|b: bool| {
        move |s: &str| {
            if b {
                StateType::Accept(s.to_string())
            } else {
                StateType::Reject(s.to_string())
            }
        }
    })
    .ap(string_p("reject")
        .fmap(|_| false)
        .or(string_p("accept").fmap(|_| true)))
    .ap(ws2()
        .right(string_pe("state"))
        .right(ws2())
        .right(word_pe().not_null_e("Expected state name"))
        .left(ws0())
        .left(char_pe(';')));
    let initial_p = || (string_p("initial").right(ws2()).fmap(|_| true)).or(ws1().fmap(|_| false));
    let arrow_p = fmake(move |b: bool| {
        move |from: &'a str| {
            move |to: &'a str| {
                StateType::State(
                    from.to_string(),
                    State {
                        initial: b,
                        transitions: Box::new(vec![Transition {
                            read_symbol: '_',
                            write_symbol: '_',
                            move_symbol: Move::Neutral,
                            new_state: to.to_string(),
                        }]),
                    },
                )
            }
        }
    })
    .ap(initial_p())
    .ap(string_pe("state")
        .right(ws2())
        .right(word_pe().not_null_e("Expected state name")))
    .ap(ws1()
        .right(string_p("->"))
        .right(ws1())
        .right(word_pe().not_null_e("Expected state name"))
        .left(ws0())
        .left(char_pe(';')));
    let tr_p = ws3()
        .right(not_char_p('}').right(transition_pe()).left(ws3()).many())
        .not_null_e("States can't have 0 transitions");
    let normal_p = fmake(move |b| {
        move |from: &'a str| {
            move |trans| {
                StateType::State(
                    from.to_string(),
                    State {
                        initial: b,
                        transitions: Box::new(trans),
                    },
                )
            }
        }
    })
    .ap(initial_p())
    .ap(string_pe("state")
        .right(ws2())
        .right(word_pe().not_null_e("Expected state name")))
    .ap(ws1().right(char_pe('{')).right(tr_p).left(char_pe('}')));
    final_p.or(arrow_p).or(normal_p)
}

// parse list of &str separated by the parser argument
fn sep_by<'a, B: 'a>(
    p1: Parser<'a, &'a str>,
    p2: Parser<'a, &'a str>,
    sep: Parser<'a, B>,
) -> Parser<'a, Vec<&'a str>> {
    fmake(move |element: &'a str| {
        move |mut list: Vec<&'a str>| {
            list.insert(0, element);
            list
        }
    })
    .ap(p1)
    .ap(sep.right(p2).many())
    .or(Parser {
        parse: Box::new(move |input| Some((input, Ok(vec![])))),
    })
}

// parse list of (&str, &str) separated by the parser argument
fn sep_by2<'a, B: 'a>(
    p1: Parser<'a, (&'a str, &'a str)>,
    p2: Parser<'a, (&'a str, &'a str)>,
    sep: Parser<'a, B>,
) -> Parser<'a, Vec<(&'a str, &'a str)>> {
    fmake(move |element: (&'a str, &'a str)| {
        move |mut list: Vec<(&'a str, &'a str)>| {
            list.insert(0, element);
            list
        }
    })
    .ap(p1)
    .ap(sep.right(p2).many())
    .or(Parser {
        parse: Box::new(move |input| Some((input, Ok(vec![])))),
    })
}

// used as separator
fn comma_p<'a>() -> Parser<'a, char> {
    ws1().right(char_p(',')).left(ws1())
}

// a single pair of two string
fn pair_p<'a>() -> Parser<'a, (&'a str, &'a str)> {
    fmake(move |first: &'a str| move |second: &'a str| (first, second))
        .ap(word_pe().not_null())
        .ap(ws2().right(word_pe()))
}

// pair parser that raises a component related error
fn pair_pe<'a>() -> Parser<'a, (&'a str, &'a str)> {
    fmake(move |first: &'a str| move |second: &'a str| (first, second))
        .ap(word_pe().not_null_e("Expected component type"))
        .ap(ws2().right(word_pe().not_null_e("Expected component name")))
}

// parser for machine
fn machine_pe<'a>() -> Parser<'a, AutomatonType> {
    fmake(move |name: &'a str| {
        move |components: Vec<(&'a str, &'a str)>| {
            move |states| {
                AutomatonType::Machine(
                    name.to_string(),
                    Machine {
                        components: Box::new(
                            components
                                .iter()
                                .map(|(c_type, c_name)| (c_type.to_string(), c_name.to_string()))
                                .collect(),
                        ),
                        states: Box::new(states),
                    },
                )
            }
        }
    })
    .ap(string_p("automaton")
        .right(ws2())
        .right(word_pe().not_null_e("Expected automaton name"))
        .left(ws1()))
    .ap(not_char_p('=')
        .right(char_pe('('))
        .right(ws1())
        .right(sep_by2(pair_p(), pair_pe(), comma_p()))
        .left(ws1())
        .left(char_pe(')'))
        .left(ws1()))
    .ap(char_pe('{')
        .right(ws3())
        .right(
            not_char_p('}')
                .right(state_pe())
                .left(ws3())
                .many()
                .not_null_e("Automaton can't have 0 states"),
        )
        .left(char_pe('}')))
}

// macro for parsing numbers
fn number_pe<'a>() -> Parser<'a, u32> {
    literal_p()
        .fmap(|s| s.to_string())
        .condition(|s| s.chars().all(|c| c >= '0' && c <= '9'))
        .raise_literal("", "Expected number literal found ", "")
        .fmap(|s| s.parse::<u32>().unwrap())
}

// macro keyword parsers
fn complement_pe<'a>() -> Parser<'a, MacroType> {
    string_p("complement")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(word_pe())
        .not_null_e("Expected component name")
        .left(char_pe(')'))
        .fmap(|component: &'a str| MacroType::Complement(component.to_string()))
}

fn intersect_pe<'a>() -> Parser<'a, MacroType> {
    string_p("intersect")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(sep_by(
            word_pe().not_null_e("Expected component list"),
            word_pe(),
            comma_p(),
        ))
        .condition_e(|v| v.len() > 1, "Expected two or more components")
        .left(char_pe(')'))
        .fmap(|components| {
            MacroType::Intersect(Box::new(components.iter().map(|c| c.to_string()).collect()))
        })
}

fn reunion_pe<'a>() -> Parser<'a, MacroType> {
    string_p("reunion")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(sep_by(
            word_pe().not_null_e("Expected component list"),
            word_pe(),
            comma_p(),
        ))
        .condition_e(|v| v.len() > 1, "Expected two or more components")
        .left(char_pe(')'))
        .fmap(|components| {
            MacroType::Reunion(Box::new(components.iter().map(|c| c.to_string()).collect()))
        })
}

fn chain_pe<'a>() -> Parser<'a, MacroType> {
    string_p("chain")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(sep_by(
            word_pe().not_null_e("Expected component list"),
            word_pe(),
            comma_p(),
        ))
        .condition_e(|v| v.len() > 1, "Expected two or more components")
        .left(char_pe(')'))
        .fmap(|components| {
            MacroType::Chain(Box::new(components.iter().map(|c| c.to_string()).collect()))
        })
}

fn repeat_pe<'a>() -> Parser<'a, MacroType> {
    fmake(move |num| move |component: &'a str| MacroType::Repeat(num, component.to_string()))
        .ap(string_p("repeat")
            .right(ws1())
            .right(char_pe('('))
            .right(ws1())
            .right(number_pe())
            .left(ws1()))
        .ap(char_pe(',')
            .right(ws1())
            .right(word_pe().not_null_e("Expected component name"))
            .left(ws1())
            .left(char_pe(')')))
}

fn move_mpe<'a>() -> Parser<'a, MacroType> {
    fmake(move |move_symbol| move |num| MacroType::Move(move_symbol, num))
        .ap(string_p("move")
            .right(ws1())
            .right(char_pe('('))
            .right(ws1())
            .right(move_pe())
            .left(ws1()))
        .ap(char_pe(',')
            .right(ws1())
            .right(number_pe())
            .left(ws1())
            .left(char_pe(')')))
}

fn override_pe<'a>() -> Parser<'a, MacroType> {
    fmake(move |move_symbol| {
        move |num| move |tape_symbol| MacroType::Override(move_symbol, num, tape_symbol)
    })
    .ap(string_p("override")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(move_pe())
        .left(ws1()))
    .ap(char_pe(',').right(ws1()).right(number_pe()).left(ws1()))
    .ap(char_pe(',')
        .right(ws1())
        .right(char_pe('\''))
        .right(symbol_pe())
        .left(char_pe('\''))
        .left(ws1())
        .left(char_pe(')')))
}

fn place_pe<'a>() -> Parser<'a, MacroType> {
    string_p("place")
        .right(ws1())
        .right(char_pe('('))
        .right(ws1())
        .right(char_pe('\"'))
        .right(tape_pe())
        .left(char_pe('\"'))
        .left(ws1())
        .left(char_pe(')'))
        .fmap(move |content: &'a str| MacroType::Place(content.to_string()))
}

fn shift<'a>() -> Parser<'a, MacroType> {
    fmake(move |move_symbol| move |num| MacroType::Shift(move_symbol, num))
        .ap(string_p("shift")
            .right(ws1())
            .right(char_pe('('))
            .right(ws1())
            .right(move_pe())
            .left(ws1()))
        .ap(char_pe(',')
            .right(ws1())
            .right(number_pe())
            .left(ws1())
            .left(char_pe(')')))
}

// macro type parser
fn macro_pe<'a>() -> Parser<'a, AutomatonType> {
    fmake(move |name: &'a str| move |macro_type| AutomatonType::Macro(name.to_string(), macro_type))
        .ap(string_p("automaton")
            .right(ws2())
            .right(word_pe())
            .not_null_e("Expected automaton name"))
        .left(ws1())
        .left(char_p('='))
        .left(ws1())
        .ap(complement_pe()
            .or(intersect_pe())
            .or(reunion_pe())
            .or(chain_pe())
            .or(repeat_pe())
            .or(move_mpe())
            .or(override_pe())
            .or(place_pe())
            .or(shift())
            .raise_literal("", "Expected macro keyword found ", ""))
        .left(ws0())
        .left(char_pe(';'))
}

// parser for automaton
fn automaton_pe<'a>() -> Parser<'a, AutomatonType> {
    macro_pe().or(machine_pe())
}

// make sure there's no more input left to consume
fn done_p<'a>() -> Parser<'a, ()> {
    Parser {
        parse: Box::new(move |input| match input.look() {
            Some(_) => None,
            _ => Some((input, Ok(()))),
        }),
    }
}

// parser for program
fn program_pe<'a>() -> Parser<'a, Program> {
    ws3()
        .right(automaton_pe())
        .left(ws3())
        .many()
        .left(done_p().raise_literal("", "Expected automaton found unexpected keyword ", ""))
        .fmap(move |automata| Program {
            automata: Box::new(automata),
        })
}

// Exposed functions for parsing code

pub fn parse_move(input: &str) -> Result<Move, String> {
    match (move_pe().parse)(Leftover {
        input,
        row: 0,
        col: 0,
    }) {
        None => panic!("move parser should never return none"),
        Some((_, res)) => res,
    }
}

pub fn parse_transition(input: &str) -> Result<Transition, String> {
    match (transition_pe().parse)(Leftover {
        input,
        row: 0,
        col: 0,
    }) {
        None => panic!("transition parser should never return none"),
        Some((_, res)) => res,
    }
}

pub fn parse_state(input: &str) -> Result<StateType, String> {
    match (state_pe().parse)(Leftover {
        input,
        row: 0,
        col: 0,
    }) {
        None => panic!("state parser should never return none"),
        Some((_, res)) => res,
    }
}

pub fn parse_automaton(input: &str) -> Result<AutomatonType, String> {
    match (automaton_pe().parse)(Leftover {
        input,
        row: 0,
        col: 0,
    }) {
        None => panic!("automaton parser should never return none"),
        Some((_, res)) => res,
    }
}

pub fn parse_program(input: &str) -> Result<Program, (Leftover, String)> {
    match (program_pe().parse)(Leftover {
        input,
        row: 0,
        col: 0,
    }) {
        None => panic!("program parser should never return none"),
        Some((leftover, res)) => match res {
            Ok(prog) => Ok(prog),
            Err(err) => Err((leftover, err)),
        },
    }
}
