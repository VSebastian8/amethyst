#[cfg(test)]
mod advanced_tests {
    use amethyst::advanced_parser::{
        parse_automaton, parse_move, parse_program, parse_state, parse_transition, Leftover,
    };
    use amethyst::syntax::*;

    #[test]
    pub fn test_moves() {
        assert_eq!(parse_move("L"), Ok(Move::Left));
        assert_eq!(parse_move("R"), Ok(Move::Right));
        assert_eq!(parse_move("N"), Ok(Move::Neutral));
        assert_eq!(
            parse_move("whatever"),
            Err("Expected move symbol found w".to_string())
        );
    }
    #[test]
    pub fn test_transitions() {
        assert_eq!(
            parse_transition(" A / B , R -> qstare ;  "),
            Ok(Transition {
                read_symbol: 'A',
                write_symbol: 'B',
                move_symbol: Move::Right,
                new_state: "qstare".to_string()
            })
        );
        assert_eq!(
            parse_transition("X/_,N->q2;"),
            Ok(Transition {
                read_symbol: 'X',
                write_symbol: '_',
                move_symbol: Move::Neutral,
                new_state: "q2".to_string()
            })
        );
        assert_eq!(
            parse_transition("Y/,,L->q3"),
            Err("Expected tape symbol found ,".to_string())
        );
        assert_eq!(
            parse_transition("AB/CD , R -> q41;"),
            Err("Expected / found B".to_string())
        );
        assert_eq!(
            parse_transition("B/B,C -> q"),
            Err("Expected move symbol found C".to_string())
        );
        assert_eq!(
            parse_transition("A/AA,L -> we"),
            Err("Expected , found A".to_string())
        );
        assert_eq!(
            parse_transition("A/B,R-> starE;"),
            Err("Forbidden symbol in word starE".to_string())
        );
        assert_eq!(
            parse_transition("A/B,N -> qstare "),
            Err("Expected ; found no more input".to_string())
        );
    }

    #[test]
    pub fn test_states() {
        assert_eq!(
            parse_state("initial state nume{ B / B, L -> nume;} "),
            Ok(StateType::State(
                "nume".to_string(),
                State {
                    initial: true,
                    transitions: Box::new(vec![Transition {
                        read_symbol: 'B',
                        write_symbol: 'B',
                        move_symbol: Move::Left,
                        new_state: "nume".to_string()
                    }])
                }
            ))
        );
        assert_eq!(
            parse_state("accept state okk;"),
            Ok(StateType::Accept("okk".to_string()))
        );
        assert_eq!(
            parse_state("reject state nu_okk;"),
            Ok(StateType::Reject("nu_okk".to_string()))
        );
        assert_eq!(
            parse_state("state renume {{-state begins-}A/A,N->nume; --a transition\n {-comment\nin\nstate-}  H/B,R->renume;}"),
            Ok(StateType::State("renume".to_string(), State { initial: false, transitions: Box::new(vec![Transition{
                read_symbol: 'A',
                write_symbol: 'A',
                move_symbol: Move::Neutral,
                new_state: "nume".to_string()
            },Transition {
                read_symbol: 'H',
                write_symbol: 'B',
                move_symbol: Move::Right,
                new_state: "renume".to_string()
            }]) }))
        );
        assert_eq!(
            parse_state("state first -> second;"),
            Ok(StateType::State(
                "first".to_string(),
                State {
                    initial: false,
                    transitions: Box::new(vec![Transition {
                        read_symbol: '_',
                        write_symbol: '_',
                        move_symbol: Move::Neutral,
                        new_state: "second".to_string()
                    }])
                }
            ))
        );
        assert_eq!(
            parse_state("initial state q0 -> x.accept;"),
            Ok(StateType::State(
                "q0".to_string(),
                State {
                    initial: true,
                    transitions: Box::new(vec![Transition {
                        read_symbol: '_',
                        write_symbol: '_',
                        move_symbol: Move::Neutral,
                        new_state: "x.accept".to_string()
                    }])
                }
            ))
        );
        assert_eq!(
            parse_state("acceptstate bad;"),
            Err("Expected space".to_string())
        );
        assert_eq!(
            parse_state("reject statebad;"),
            Err("Expected space".to_string())
        );
        assert_eq!(
            parse_state("state {A/A,R->q;}"),
            Err("Expected state name".to_string())
        );
        assert_eq!(
            parse_state("initial state q;"),
            Err("Expected { found ;".to_string())
        );
        assert_eq!(
            parse_state("initial state q2{}"),
            Err("States can't have 0 transitions".to_string())
        );
        assert_eq!(
            parse_state("state ab -> Bcd"),
            Err("Forbidden symbol in word Bcd".to_string())
        );
        assert_eq!(
            parse_state("state ab -> ;"),
            Err("Expected state name".to_string())
        );
        assert_eq!(
            parse_state("initial q0 -> q1;"),
            Err("Expected state found q0".to_string())
        );
    }

    #[test]
    pub fn test_machines() {
        assert_eq!(
            parse_automaton("automaton or(not n1){initial state q0 {0 / 0, R -> q0; 1 / 1 , R -> q1;} state q1 {0 / 1 , R -> q1; 1 / 1 , N -> q1; _ / _, N -> f;} accept state f;}"),
            Ok(AutomatonType::Machine(
                "or".to_string(),
                Machine {
                    components: Box::new(vec![("not".to_string(), "n1".to_string())]),
                    states: Box::new(vec![
                        StateType::State(
                            "q0".to_string(),
                            State{
                                initial: true,
                                transitions:
                                    Box::new(vec![
                                        Transition{read_symbol: '0', write_symbol:'0', move_symbol: Move::Right, new_state: "q0".to_string()},
                                        Transition{read_symbol: '1', write_symbol: '1', move_symbol: Move::Right, new_state: "q1".to_string()}
                                    ])
                            }
                        ),
                         StateType::State(
                            "q1".to_string(),
                            State{
                                initial: false,
                                transitions:
                                    Box::new(vec![
                                        Transition{read_symbol: '0', write_symbol:'1', move_symbol: Move::Right, new_state: "q1".to_string()},
                                        Transition{read_symbol: '1', write_symbol: '1', move_symbol: Move::Neutral, new_state: "q1".to_string()},
                                        Transition{read_symbol: '_', write_symbol: '_', move_symbol: Move::Neutral, new_state: "f".to_string()}
                                    ])
                            }
                        ),
                        StateType::Accept("f".to_string())
                    ])
                }
            ))
        );
        assert_eq!(
            parse_automaton("automaton main{dsad}"),
            Err("Expected ( found {".to_string())
        );
        assert_eq!(
            parse_automaton("automaton main(c1 c2){}"),
            Err("Automaton can't have 0 states".to_string())
        );
        assert_eq!(
            parse_automaton("automaton main(){ initial state q0;}"),
            Err("Expected { found ;".to_string())
        );
    }

    #[test]
    fn test_macros() {
        // Complement
        assert_eq!(
            parse_automaton("automaton comp = complement(and);"),
            Ok(AutomatonType::Macro(
                "comp".to_string(),
                MacroType::Complement("and".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton comp = complement{and};"),
            Err("Expected ( found {".to_string())
        );
        assert_eq!(
            parse_automaton("automaton comp = complement();"),
            Err("Expected component name".to_string())
        );
        // Intersect
        assert_eq!(
            parse_automaton("automaton int = intersect(not, not, not);"),
            Ok(AutomatonType::Macro(
                "int".to_string(),
                MacroType::Intersect(Box::new(vec![
                    "not".to_string(),
                    "not".to_string(),
                    "not".to_string()
                ]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton int = intersect();"),
            Err("Expected component list".to_string())
        );
        assert_eq!(
            parse_automaton("automaton int = intersect(not);"),
            Err("Expected two or more components".to_string())
        );
        // Reunion
        assert_eq!(
            parse_automaton("automaton ren = reunion(and, or);"),
            Ok(AutomatonType::Macro(
                "ren".to_string(),
                MacroType::Reunion(Box::new(vec!["and".to_string(), "or".to_string()]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton = reunion(not);"),
            Err("Expected automaton name".to_string())
        );
        // Chain
        assert_eq!(
            parse_automaton("automaton lant = chain(not, or, move2l);"),
            Ok(AutomatonType::Macro(
                "lant".to_string(),
                MacroType::Chain(Box::new(vec![
                    "not".to_string(),
                    "or".to_string(),
                    "move2l".to_string()
                ]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton l = chain(notB);"),
            Err("Forbidden symbol in word notB".to_string())
        );
        assert_eq!(
            parse_automaton("automaton ch = chain();"),
            Err("Expected component list".to_string())
        );
        // Repeat
        assert_eq!(
            parse_automaton("automaton repetare = repeat(5, not);"),
            Ok(AutomatonType::Macro(
                "repetare".to_string(),
                MacroType::Repeat(5, "not".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton yo_gurt = repeat(007, hii);"),
            Ok(AutomatonType::Macro(
                "yo_gurt".to_string(),
                MacroType::Repeat(7, "hii".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton wrong_num = repeat(12ab,);"),
            Err("Expected number literal found 12ab".to_string())
        );
        assert_eq!(
            parse_automaton("automaton bad = repeat(hello);"),
            Err("Expected number literal found hello".to_string())
        );
        // Move
        assert_eq!(
            parse_automaton("automaton move8r = move(R, 8);"),
            Ok(AutomatonType::Macro(
                "move8r".to_string(),
                MacroType::Move(Move::Right, 8)
            ))
        );
        assert_eq!(
            parse_automaton("automaton bad = move(3, L);"),
            Err("Expected move symbol found 3".to_string())
        );
        assert_eq!(
            parse_automaton("automaton bad = move(L 4);"),
            Err("Expected , found 4".to_string())
        );
        // Override
        assert_eq!(
            parse_automaton("automaton rescriere = override(L, 5, 'V');"),
            Ok(AutomatonType::Macro(
                "rescriere".to_string(),
                MacroType::Override(Move::Left, 5, 'V')
            ))
        );
        assert_eq!(
            parse_automaton("automaton o7 = override(R, 7, X);"),
            Err("Expected ' found X".to_string())
        );
        assert_eq!(
            parse_automaton("automaton o7 = override(R, 7, 'a');"),
            Err("Expected tape symbol found a".to_string())
        );
        // Place
        assert_eq!(
            parse_automaton("automaton scriere = place(\"ABCDE\");"),
            Ok(AutomatonType::Macro(
                "scriere".to_string(),
                MacroType::Place("ABCDE".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton pp = place(\"ABcDE\");"),
            Err("Forbidden symbol in tape sequence ABcDE".to_string())
        );
        // Shift
        assert_eq!(
            parse_automaton("automaton insert8 = shift(R, 8);"),
            Ok(AutomatonType::Macro(
                "insert8".to_string(),
                MacroType::Shift(Move::Right, 8)
            ))
        );
        assert_eq!(
            parse_automaton("automaton delete19 = shift(L, 19);"),
            Ok(AutomatonType::Macro(
                "delete19".to_string(),
                MacroType::Shift(Move::Left, 19)
            ))
        );
    }
    #[test]
    pub fn test_program() {
        assert_eq!(
            parse_program(
                "  automaton not(){    
       initial state q0 {     
           @ / @ , R -> q0;   
           1 / 0 , L -> q1;   
           0 / 1 , L -> q1;   
       }   
       accept state q1;
   }
   -- Here is a comment in the program
   {-And another-}--and  another immediately after
   automaton three = repeat(3, not); -- this is a macro
   automaton main (not n1, three n2) {
       initial state q0 {
           B/B,N-> n1.q0; _/B,R->qrej;
       }
       state n1.q1 {_/_,N->n2.q0;}
       state n2.q1 {
           _ / _ , N -> qacc;
       }
       reject state qrej; accept state qacc;
   } -- program done"
            ),
            Ok(Program {
                automata: Box::new(vec![
                    AutomatonType::Machine(
                        "not".to_owned(),
                        Machine {
                            components: Box::new(vec![]),
                            states: Box::new(vec![
                                StateType::State(
                                    "q0".to_owned(),
                                    State {
                                        initial: true,
                                        transitions: Box::new(vec![
                                            Transition {
                                                read_symbol: '@',
                                                write_symbol: '@',
                                                move_symbol: Move::Right,
                                                new_state: "q0".to_owned()
                                            },
                                            Transition {
                                                read_symbol: '1',
                                                write_symbol: '0',
                                                move_symbol: Move::Left,
                                                new_state: "q1".to_owned()
                                            },
                                            Transition {
                                                read_symbol: '0',
                                                write_symbol: '1',
                                                move_symbol: Move::Left,
                                                new_state: "q1".to_owned()
                                            }
                                        ])
                                    }
                                ),
                                StateType::Accept("q1".to_owned())
                            ])
                        }
                    ),
                    AutomatonType::Macro(
                        "three".to_owned(),
                        MacroType::Repeat(3, "not".to_owned())
                    ),
                    AutomatonType::Machine(
                        "main".to_owned(),
                        Machine {
                            components: Box::new(vec![
                                ("not".to_owned(), "n1".to_owned()),
                                ("three".to_owned(), "n2".to_owned())
                            ]),
                            states: Box::new(vec![
                                StateType::State(
                                    "q0".to_owned(),
                                    State {
                                        initial: true,
                                        transitions: Box::new(vec![
                                            Transition {
                                                read_symbol: 'B',
                                                write_symbol: 'B',
                                                move_symbol: Move::Neutral,
                                                new_state: "n1.q0".to_owned()
                                            },
                                            Transition {
                                                read_symbol: '_',
                                                write_symbol: 'B',
                                                move_symbol: Move::Right,
                                                new_state: "qrej".to_owned()
                                            }
                                        ])
                                    }
                                ),
                                StateType::State(
                                    "n1.q1".to_owned(),
                                    State {
                                        initial: false,
                                        transitions: Box::new(vec![Transition {
                                            read_symbol: '_',
                                            write_symbol: '_',
                                            move_symbol: Move::Neutral,
                                            new_state: "n2.q0".to_owned()
                                        }])
                                    }
                                ),
                                StateType::State(
                                    "n2.q1".to_owned(),
                                    State {
                                        initial: false,
                                        transitions: Box::new(vec![Transition {
                                            read_symbol: '_',
                                            write_symbol: '_',
                                            move_symbol: Move::Neutral,
                                            new_state: "qacc".to_owned()
                                        }])
                                    }
                                ),
                                StateType::Reject("qrej".to_owned()),
                                StateType::Accept("qacc".to_owned())
                            ])
                        }
                    )
                ])
            })
        );

        assert_eq!(
            parse_program(
                "automaton ch = chain(c1, c2);

auto
automaton a(){}
"
            ),
            Err((
                Leftover {
                    input: "auto\nautomaton a(){}\n",
                    row: 2,
                    col: 0
                },
                "Expected automaton found unexpected keyword auto".to_owned()
            ))
        );

        assert_eq!(
            parse_program(
                "
                automaton output = place(\"HELLO-WORLD!\");
                automaton mv = move(R, 12);
                automaton place_and_move = chain(output, mv);
                automaton do3 = repeat(3, place_and_move);
                automaton go.back = move(L, 36); --hi
                {-and now for the main-event-}
                automaton main = chain(do3, go.back);"
            ),
            Ok(Program {
                automata: Box::new(vec![
                    AutomatonType::Macro(
                        "output".to_owned(),
                        MacroType::Place("HELLO-WORLD!".to_owned())
                    ),
                    AutomatonType::Macro("mv".to_owned(), MacroType::Move(Move::Right, 12)),
                    AutomatonType::Macro(
                        "place_and_move".to_owned(),
                        MacroType::Chain(Box::new(vec!["output".to_owned(), "mv".to_owned()]))
                    ),
                    AutomatonType::Macro(
                        "do3".to_owned(),
                        MacroType::Repeat(3, "place_and_move".to_owned())
                    ),
                    AutomatonType::Macro("go.back".to_owned(), MacroType::Move(Move::Left, 36)),
                    AutomatonType::Macro(
                        "main".to_owned(),
                        MacroType::Chain(Box::new(vec!["do3".to_owned(), "go.back".to_owned()]))
                    )
                ])
            })
        );
    }
}
