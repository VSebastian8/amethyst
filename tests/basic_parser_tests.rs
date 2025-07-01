
#[cfg(test)]
mod basic_tests {
    use amethyst::syntax::*;
    use amethyst::basic_parser::{parse_automaton, parse_state, parse_transition,parse_move};

    #[test]
    pub fn test_moves() {
        assert_eq!(parse_move("L"), Some(Move::Left));
        assert_eq!(parse_move("R"), Some(Move::Right));
        assert_eq!(parse_move("N"), Some(Move::Neutral));
        assert_eq!(parse_move("whatever"), None);
    }

    #[test]
    pub fn test_transitions() {
        assert_eq!(
            parse_transition(" A / B , R -> qstare ;  "),
            Some(Transition {
                read_symbol: 'A',
                write_symbol: 'B',
                move_symbol: Move::Right,
                new_state: "qstare".to_string()
            })
        );
        assert_eq!(
            parse_transition("X/_,N->q2;"),
            Some(Transition {
                read_symbol: 'X',
                write_symbol: '_',
                move_symbol: Move::Neutral,
                new_state: "q2".to_string()
            })
        );
        assert_eq!(parse_transition("Y/,,L->q3"), None);
        assert_eq!(parse_transition("AB/CD , R -> q41;"), None);
        assert_eq!(parse_transition("B/B,C -> q"), None);
        assert_eq!(parse_transition("A/AA,L -> we"), None);
        assert_eq!(parse_transition("A/B,R-> starE;"), None);
        assert_eq!(parse_transition("A/B,N -> qstare "), None);
    }

    #[test]
    pub fn test_states() {
        assert_eq!(
            parse_state("initial state nume{ B / B, L -> nume;} "),
            Some(StateType::State(
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
            Some(StateType::Accept("okk".to_string()))
        );
        assert_eq!(
            parse_state("reject state nu_okk;"),
            Some(StateType::Reject("nu_okk".to_string()))
        );
        assert_eq!(parse_state("acceptstate bad;"), None);
        assert_eq!(parse_state("reject statebad;"), None);
        assert_eq!(parse_state("state {A/A,R->q;}"), None);
        assert_eq!(parse_state("initial state q;"), None);
        assert_eq!(parse_state("initial state q2{}"), None);
    }

    #[test]
    pub fn test_macros() {
        assert_eq!(
            parse_automaton("automaton comp = complement(and);"),
            Some(AutomatonType::Macro(
                "comp".to_string(),
                MacroType::Complement("and".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton int = intersect(not, not, not);"),
            Some(AutomatonType::Macro(
                "int".to_string(),
                MacroType::Intersect(Box::new(vec![
                    "not".to_string(),
                    "not".to_string(),
                    "not".to_string()
                ]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton ren = reunion(and, or);"),
            Some(AutomatonType::Macro(
                "ren".to_string(),
                MacroType::Reunion(Box::new(vec!["and".to_string(), "or".to_string()]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton lant = chain(not, or, move2l);"),
            Some(AutomatonType::Macro(
                "lant".to_string(),
                MacroType::Chain(Box::new(vec![
                    "not".to_string(),
                    "or".to_string(),
                    "move2l".to_string()
                ]))
            ))
        );
        assert_eq!(
            parse_automaton("automaton repetare = repeat(not, 5);"),
            Some(AutomatonType::Macro(
                "repetare".to_string(),
                MacroType::Repeat("not".to_string(), 5)
            ))
        );
        assert_eq!(
            parse_automaton("automaton move8r = move(R, 8);"),
            Some(AutomatonType::Macro(
                "move8r".to_string(),
                MacroType::Move(Move::Right, 8)
            ))
        );
        assert_eq!(
            parse_automaton("automaton rescriere = override(L, 5, 'V');"),
            Some(AutomatonType::Macro(
                "rescriere".to_string(),
                MacroType::Override(Move::Left, 5, 'V')
            ))
        );
        assert_eq!(
            parse_automaton("automaton scriere = place(\"ABCDE\");"),
            Some(AutomatonType::Macro(
                "scriere".to_string(),
                MacroType::Place("ABCDE".to_string())
            ))
        );
        assert_eq!(
            parse_automaton("automaton insert8 = shift(R, 8);"),
            Some(AutomatonType::Macro(
                "insert8".to_string(),
                MacroType::Shift(Move::Right, 8)
            ))
        );
        assert_eq!(
            parse_automaton("automaton delete19 = shift(L, 19);"),
            Some(AutomatonType::Macro(
                "delete19".to_string(),
                MacroType::Shift(Move::Left, 19)
            ))
        );
        assert_eq!(parse_automaton("automaton bad = repeat(hello);"), None);
        assert_eq!(parse_automaton("automaton bad = move(3, L);"), None);
        assert_eq!(parse_automaton("automaton ch = chain();"), None);
    }

    #[test]
    pub fn test_machines() {
        assert_eq!(
            parse_automaton("automaton or(not n1){initial state q0 {0 / 0, R -> q0; 1 / 1 , R -> q1;} state q1 {0 / 1 , R -> q1; 1 / 1 , N -> q1; _ / _, N -> f;} accept state f;}"),
            Some(AutomatonType::Machine(
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
        assert_eq!(parse_automaton("automaton main{}"), None);
        assert_eq!(parse_automaton("automaton main(c1,c2){}"), None);
        assert_eq!(parse_automaton("automaton main(){ initial state q0;}"), None);
    }
}
