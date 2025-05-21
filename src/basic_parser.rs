use crate::syntax::{AutomatonType, Machine, MacroType, Program};
use crate::syntax::{Move, State};
use crate::syntax::{StateType, Transition};

const ALLOWED_NAME_SYMBOLS: &str = "abcdefghijklmnopqrstuvwxyz0123456789_.";
const ALLOWED_TAPE_SYMBOLS: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*[]-+=/?_:";

/// Use string slices (`&str`) instead of `String` for more efficient parsing.
enum Parser<'a, T> {
    Parser(Box<dyn Fn(&'a str) -> Option<(&'a str, T)> + 'a>),
}

fn fmap<'a, A: 'a, B: 'a, F>(f: F, p: Parser<'a, A>) -> Parser<'a, B>
where
    F: Fn(A) -> B + 'a,
{
    match p {
        Parser::Parser(parse) => Parser::Parser(Box::new(move |input| match parse(input) {
            None => None,
            Some((input2, x)) => Some((input2, f(x))),
        })),
    }
}

fn char_p<'a>(x: char) -> Parser<'a, char> {
    Parser::Parser(Box::new(move |input: &'a str| {
        let mut chars = input.char_indices();
        match chars.next() {
            Some((_, c)) if c == x => {
                let next_index = c.len_utf8();
                Some((&input[next_index..], c))
            }
            _ => None,
        }
    }))
}
fn pure<'a, T: Clone + 'a>(x: T) -> Parser<'a, T> {
    Parser::Parser(Box::new(move |input| Some((input, x.clone()))))
}

fn ap<'a, A: 'a, B: 'a, F>(pf: Parser<'a, F>, pa: Parser<'a, A>) -> Parser<'a, B>
where
    F: Fn(A) -> B + 'a,
{
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref pf_inner) = pf;
        let Parser::Parser(ref pa_inner) = pa;
        pf_inner(input).and_then(|(input1, f)| pa_inner(input1).map(|(input2, x)| (input2, f(x))))
    }))
}

fn empty<'a, T: 'a>() -> Parser<'a, T> {
    Parser::Parser(Box::new(|_| None))
}

fn or<'a, T: 'a>(p1: Parser<'a, T>, p2: Parser<'a, T>) -> Parser<'a, T> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref p1_inner) = p1;
        let Parser::Parser(ref p2_inner) = p2;
        p1_inner(input).or_else(|| p2_inner(input))
    }))
}

fn string_p<'a>(s: &'a str) -> Parser<'a, String> {
    s.chars().fold(pure(String::new()), |acc, c| {
        let f = |s: &String, ch: char| {
            let mut new_s = s.clone();
            new_s.push(ch);
            new_s
        };
        ap(
            fmap(
                move |s| {
                    let s = s.clone();
                    move |ch| f(&s, ch)
                },
                acc,
            ),
            char_p(c),
        )
    })
}

fn span_p<'a, F>(f: F) -> Parser<'a, String>
where
    F: Fn(char) -> bool + 'a,
{
    Parser::Parser(Box::new(move |input: &'a str| {
        let token: String = input.chars().take_while(|&c| f(c)).collect();
        let len = token.chars().map(|c| c.len_utf8()).sum();
        if !token.is_empty() {
            Some((&input[len..], token))
        } else {
            Some((input, String::new()))
        }
    }))
}

fn not_null<'a, T: 'a + Clone>(p: Parser<'a, Vec<T>>) -> Parser<'a, Vec<T>> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref inner) = p;
        inner(input).and_then(|(input2, xs)| {
            if xs.is_empty() {
                None
            } else {
                Some((input2, xs))
            }
        })
    }))
}

fn not_nulls<'a>(p: Parser<'a, String>) -> Parser<'a, String> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref inner) = p;
        inner(input).and_then(|(input2, xs)| {
            if xs.is_empty() {
                None
            } else {
                Some((input2, xs))
            }
        })
    }))
}

fn sep_by<'a, A: 'a, B: 'a>(sep: Parser<'a, A>, element: Parser<'a, B>) -> Parser<'a, Vec<B>> {
    let rest = Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref sep_inner) = sep;
        let Parser::Parser(ref element_inner) = element;
        let mut result = Vec::new();
        let mut current_input = input;
        if let Some((input2, first)) = element_inner(current_input) {
            result.push(first);
            current_input = input2;
            while let Some((input3, _)) = sep_inner(current_input) {
                if let Some((input4, next)) = element_inner(input3) {
                    result.push(next);
                    current_input = input4;
                } else {
                    break;
                }
            }
            Some((current_input, result))
        } else {
            Some((input, Vec::new()))
        }
    }));
    rest
}

fn ws<'a>() -> Parser<'a, String> {
    span_p(|c| c == ' ' || c == '\n')
}

fn ws2<'a>() -> Parser<'a, String> {
    fmap(
        |chars: Vec<char>| chars.into_iter().collect::<String>(),
        not_null(fmap(|s: String| s.chars().collect::<Vec<char>>(), ws())),
    )
}
fn number_p<'a>() -> Parser<'a, i32> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref inner) = span_p(|c| "0123456789".contains(c));
        inner(input).and_then(|(input2, s)| match s.parse::<i32>() {
            Ok(num) => Some((input2, num)),
            Err(_) => None,
        })
    }))
}

fn word_p<'a>() -> Parser<'a, String> {
    span_p(|c| ALLOWED_NAME_SYMBOLS.contains(c))
}

fn tape_p<'a>() -> Parser<'a, String> {
    span_p(|c| ALLOWED_TAPE_SYMBOLS.contains(c))
}

fn symbol_p<'a>() -> Parser<'a, char> {
    ALLOWED_TAPE_SYMBOLS
        .chars()
        .fold(empty(), |acc, c| or(acc, char_p(c)))
}

fn move_p<'a>() -> Parser<'a, Move> {
    or(
        fmap(|_| Move::Left, char_p('L')),
        or(
            fmap(|_| Move::Right, char_p('R')),
            fmap(|_| Move::Neutral, char_p('N')),
        ),
    )
}
fn transition_p<'a>() -> Parser<'a, Transition> {
    fmap(
        |(s, (t, (m, n)))| Transition {
            read_symbol: s,
            write_symbol: t,
            move_symbol: m,
            new_state: n,
        },
        ap(
            ap(
                ap(
                    fmap(
                        |s| move |t| move |m| move |n| (s, (t, (m, n))),
                        ws().and_then(symbol_p())
                            .and_keep(ws())
                            .and_keep(char_p('/')),
                    ),
                    ws().and_then(symbol_p())
                        .and_keep(ws())
                        .and_keep(char_p(',')),
                ),
                ws().and_then(move_p())
                    .and_keep(ws())
                    .and_keep(string_p("->")),
            ),
            ws().and_then(word_p()).and_keep(ws()).and_keep(char_p(';')),
        ),
    )
}

fn some<'a, T: 'a + Clone>(p: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    not_null(many(p))
}

fn many<'a, T: 'a>(p: Parser<'a, T>) -> Parser<'a, Vec<T>> {
    Parser::Parser(Box::new(move |mut input| {
        let Parser::Parser(ref inner) = p;
        let mut result = Vec::new();
        while let Some((input2, x)) = inner(input) {
            result.push(x);
            input = input2;
        }
        Some((input, result))
    }))
}

fn and_then<'a, A: 'a, B: 'a>(self_p: Parser<'a, A>, next: Parser<'a, B>) -> Parser<'a, B> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref self_inner) = self_p;
        self_inner(input).and_then(|(input2, _)| {
            let Parser::Parser(ref next_inner) = next;
            next_inner(input2)
        })
    }))
}

fn and_keep<'a, A: 'a + Clone, B: 'a>(self_p: Parser<'a, A>, next: Parser<'a, B>) -> Parser<'a, A> {
    Parser::Parser(Box::new(move |input| {
        let Parser::Parser(ref self_inner) = self_p;
        self_inner(input).and_then(|(input2, a)| {
            let Parser::Parser(ref next_inner) = next;
            next_inner(input2).map(|(input3, _)| (input3, a.clone()))
        })
    }))
}

trait ParserExt<'a, T: 'a> {
    fn and_then<B: 'a>(self, next: Parser<'a, B>) -> Parser<'a, B>;
    fn and_keep<B: 'a>(self, next: Parser<'a, B>) -> Parser<'a, T>
    where
        T: Clone;
}
impl<'a, T: 'a> ParserExt<'a, T> for Parser<'a, T> {
    fn and_then<B: 'a>(self, next: Parser<'a, B>) -> Parser<'a, B> {
        and_then(self, next)
    }
    fn and_keep<B: 'a>(self, next: Parser<'a, B>) -> Parser<'a, T>
    where
        T: Clone,
    {
        and_keep(self, next)
    }
}

fn state_p<'a>() -> Parser<'a, StateType> {
    // Helper to parse transitions block
    let make_transitions_block_p = || {
        char_p('{')
            .and_then(ws())
            .and_then(some(transition_p()))
            .and_keep(ws())
            .and_keep(char_p('}'))
    };

    // Reject state parser
    let reject_p = string_p("reject")
        .and_then(ws2())
        .and_then(string_p("state"))
        .and_then(ws2())
        .and_then(not_nulls(word_p()))
        .and_keep(ws())
        .and_keep(char_p(';'));

    // Accept state parser
    let accept_p = string_p("accept")
        .and_then(ws2())
        .and_then(string_p("state"))
        .and_then(ws2())
        .and_then(not_nulls(word_p()))
        .and_keep(ws())
        .and_keep(char_p(';'));

    // Initial state parser
    let initial_p = string_p("initial")
        .and_then(ws2())
        .and_then(string_p("state"))
        .and_then(ws2())
        .and_then(not_nulls(word_p()))
        .and_keep(ws());

    let normal_p = string_p("state")
        .and_then(ws())
        .and_then(not_nulls(word_p()))
        .and_keep(ws());

    // Compose the parser
    or(
        or(
            fmap(|name: String| StateType::Reject(name), reject_p),
            fmap(|name: String| StateType::Accept(name), accept_p),
        ),
        or(
            ap(
                fmap(
                    |name: String| {
                        let name_clone = name.clone();
                        move |transitions| {
                            StateType::State(
                                name_clone.clone(),
                                State {
                                    transitions: Box::new(transitions),
                                    initial: true,
                                },
                            )
                        }
                    },
                    initial_p,
                ),
                make_transitions_block_p(),
            ),
            ap(
                fmap(
                    |name: String| {
                        let name_clone = name.clone();
                        move |transitions| {
                            StateType::State(
                                name_clone.clone(),
                                State {
                                    transitions: Box::new(transitions),
                                    initial: false,
                                },
                            )
                        }
                    },
                    normal_p,
                ),
                make_transitions_block_p(),
            ),
        ),
    )
}

fn complement_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |name| MacroType::Complement(name),
        string_p("complement")
            .and_then(ws())
            .and_then(char_p('('))
            .and_then(ws())
            .and_then(not_nulls(word_p()))
            .and_keep(ws())
            .and_keep(char_p(')')),
    )
}

fn sep_by_word_p<'a>() -> Parser<'a, Vec<String>> {
    sep_by(
        ws().and_then(char_p(',')).and_keep(ws()),
        not_nulls(word_p()),
    )
}

fn intersect_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |names| MacroType::Intersect(Box::new(names)),
        string_p("intersect")
            .and_then(ws())
            .and_then(char_p('('))
            .and_then(ws())
            .and_then(not_null(sep_by_word_p()))
            .and_keep(ws())
            .and_keep(char_p(')')),
    )
}

fn reunion_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |names| MacroType::Reunion(Box::new(names)),
        string_p("reunion")
            .and_then(ws())
            .and_then(char_p('('))
            .and_then(ws())
            .and_then(not_null(sep_by_word_p()))
            .and_keep(ws())
            .and_keep(char_p(')')),
    )
}

fn chain_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |names| MacroType::Chain(Box::new(names)),
        string_p("chain")
            .and_then(ws())
            .and_then(char_p('('))
            .and_then(ws())
            .and_then(not_null(sep_by_word_p()))
            .and_keep(ws())
            .and_keep(char_p(')')),
    )
}

fn repeat_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |(n, name)| MacroType::Repeat(n as u32, name),
        ap(
            fmap(
                |n| move |name| (n, name),
                string_p("repeat")
                    .and_then(ws())
                    .and_then(char_p('('))
                    .and_then(ws())
                    .and_then(number_p())
                    .and_keep(ws()),
            ),
            char_p(',')
                .and_then(ws())
                .and_then(not_nulls(word_p()))
                .and_keep(ws())
                .and_keep(char_p(')')),
        ),
    )
}

fn move_mp<'a>() -> Parser<'a, MacroType> {
    fmap(
        |(mv, n)| MacroType::Move(mv, n as u32),
        ap(
            fmap(
                |mv| move |n| (mv, n),
                string_p("move")
                    .and_then(ws())
                    .and_then(char_p('('))
                    .and_then(ws())
                    .and_then(move_p())
                    .and_keep(ws()),
            ),
            char_p(',')
                .and_then(ws())
                .and_then(number_p())
                .and_keep(ws())
                .and_keep(char_p(')')),
        ),
    )
}

fn override_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |((mv, n), sym)| MacroType::Override(mv, n as u32, sym),
        ap(
            ap(
                fmap(
                    |mv| move |n| move |sym| ((mv, n), sym),
                    string_p("override")
                        .and_then(ws())
                        .and_then(char_p('('))
                        .and_then(ws())
                        .and_then(move_p())
                        .and_keep(ws()),
                ),
                char_p(',')
                    .and_then(ws())
                    .and_then(number_p())
                    .and_keep(ws())
                    .and_keep(char_p(','))
                    .and_keep(ws()),
            ),
            char_p('\'')
                .and_then(symbol_p())
                .and_keep(char_p('\''))
                .and_keep(ws())
                .and_keep(char_p(')')),
        ),
    )
}

fn place_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |tape| MacroType::Place(tape),
        string_p("place")
            .and_then(ws())
            .and_then(char_p('('))
            .and_then(ws())
            .and_then(char_p('"'))
            .and_then(tape_p())
            .and_keep(char_p('"'))
            .and_keep(ws())
            .and_keep(char_p(')')),
    )
}

fn shift_p<'a>() -> Parser<'a, MacroType> {
    fmap(
        |(mv, n)| MacroType::Shift(mv, n as u32),
        ap(
            fmap(
                |mv| move |n| (mv, n),
                string_p("shift")
                    .and_then(ws())
                    .and_then(char_p('('))
                    .and_then(ws())
                    .and_then(move_p())
                    .and_keep(ws()),
            ),
            char_p(',')
                .and_then(ws())
                .and_then(number_p())
                .and_keep(ws())
                .and_keep(char_p(')')),
        ),
    )
}

fn macro_p<'a>() -> Parser<'a, AutomatonType> {
    fmap(
        |(name, macro_kw)| AutomatonType::Macro(name, macro_kw),
        ap(
            fmap(
                |name: String| move |macro_kw| (name.clone(), macro_kw),
                string_p("automaton")
                    .and_then(ws2())
                    .and_then(not_nulls(word_p()))
                    .and_keep(ws())
                    .and_keep(char_p('='))
                    .and_keep(ws()),
            ),
            or(
                complement_p(),
                or(
                    intersect_p(),
                    or(
                        reunion_p(),
                        or(
                            chain_p(),
                            or(
                                repeat_p(),
                                or(move_mp(), or(override_p(), or(place_p(), shift_p()))),
                            ),
                        ),
                    ),
                ),
            ),
        )
        .and_keep(ws())
        .and_keep(char_p(';')),
    )
}

fn pair_p<'a>() -> Parser<'a, (String, String)> {
    ap(
        fmap(|a: String| move |b: String| (a.clone(), b), word_p()),
        ws2().and_then(word_p()),
    )
}

fn machine_p<'a>() -> Parser<'a, AutomatonType> {
    fmap(
        |(name, (pairs, states))| {
            AutomatonType::Machine(
                name,
                Machine {
                    components: Box::new(pairs),
                    states: Box::new(states),
                },
            )
        },
        ap(
            ap(
                fmap(
                    |name: String| {
                        // this is ugly, maybe there's a way around it?
                        let name_clone = name.clone();
                        move |pairs: Vec<(String, String)>| {
                            let name_clone2 = name_clone.clone();
                            let pairs_clone = pairs.clone();
                            move |states| (name_clone2.clone(), (pairs_clone.clone(), states))
                        }
                    },
                    string_p("automaton")
                        .and_then(ws2())
                        .and_then(not_nulls(word_p()))
                        .and_keep(ws()),
                ),
                char_p('(')
                    .and_then(sep_by(ws().and_then(char_p(',')).and_keep(ws()), pair_p()))
                    .and_keep(char_p(')'))
                    .and_keep(ws()),
            ),
            char_p('{')
                .and_then(ws())
                .and_then(some(state_p().and_keep(ws())))
                .and_keep(char_p('}')),
        ),
    )
}

fn automata_p<'a>() -> Parser<'a, AutomatonType> {
    or(macro_p(), machine_p())
}

fn program_p<'a>() -> Parser<'a, Program> {
    fmap(
        |automata| Program {
            automata: Box::new(automata),
        },
        many(ws().and_then(automata_p()).and_keep(ws())),
    )
}

pub fn parse_move(input: &str) -> Option<Move> {
    let Parser::Parser(parse) = move_p();
    match parse(input) {
        None => None,
        Some((_, move_symbol)) => Some(move_symbol),
    }
}

pub fn parse_transition(input: &str) -> Option<Transition> {
    let Parser::Parser(parse) = transition_p();
    match parse(input) {
        None => None,
        Some((_, transition)) => Some(transition),
    }
}

pub fn parse_state(input: &str) -> Option<StateType> {
    let Parser::Parser(parse) = state_p();
    match parse(input) {
        None => None,
        Some((_, state)) => Some(state),
    }
}

pub fn parse_automaton(input: &str) -> Option<AutomatonType> {
    let Parser::Parser(parse) = automata_p();
    match parse(input) {
        None => None,
        Some((_, automaton)) => Some(automaton),
    }
}

pub fn parse_program(input: &str) -> Option<Program> {
    let Parser::Parser(parse) = program_p();
    match parse(input) {
        None => None,
        Some((_, program)) => Some(program),
    }
}
