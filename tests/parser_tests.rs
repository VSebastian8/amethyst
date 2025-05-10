extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    // Always free memory after using it
    fn free_transition(transition_ptr: *mut i32);
    fn free_state(state_ptr: *mut i32);
    fn free_automaton(automaton_ptr: *mut i32);
    fn free_result(program_ptr: *mut i32);
    // Test functions
    fn test_transition(n: i32) -> *mut i32;
    fn test_state(n: i32) -> *mut i32;
    fn test_macro(n: i32) -> *mut i32;
    fn test_machine(n: i32) -> *mut i32;
    fn test_program(n: i32) -> *mut i32;
}

use amethyst::parser::{parse_automaton, parse_result, parse_state, parse_transition};
use serial_test::serial;

#[cfg(test)]
mod tests {
    use amethyst::syntax::{
        AutomatonType, Machine, MacroType, Move, Program, State, StateType, Transition,
    };

    use super::*;

    #[ctor::ctor]
    fn setup() {
        println!("Setup: This runs before all tests. It initializes the haskell environment");
        unsafe {
            initialize_haskell();
        }
    }

    #[ctor::dtor]
    fn teardown() {
        println!("Teardown: This runs after all tests. It closes the haskell environment");
        unsafe {
            exit_haskell();
        }
    }

    #[test]
    #[serial]
    pub fn test_transition_1() {
        let transition;
        unsafe {
            let transition_ptr = test_transition(1);
            transition = parse_transition(transition_ptr);
            free_transition(transition_ptr);
        }
        print!("{:?}", transition);
        assert_eq!(
            transition,
            Transition {
                read_symbol: 'A',
                write_symbol: 'B',
                move_symbol: Move::Right,
                new_state: "qstare".to_owned()
            }
        )
    }

    #[test]
    #[serial]
    pub fn test_transition_2() {
        let transition;
        unsafe {
            let transition_ptr = test_transition(2);
            transition = parse_transition(transition_ptr);
            free_transition(transition_ptr);
        }
        print!("{:?}", transition);
        assert_eq!(
            transition,
            Transition {
                read_symbol: 'X',
                write_symbol: '_',
                move_symbol: Move::Neutral,
                new_state: "q2".to_owned()
            }
        )
    }

    #[test]
    #[serial]
    pub fn test_state_1() {
        let state;
        unsafe {
            let state_ptr = test_state(1);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(
            state,
            StateType::State(
                "nume".to_owned(),
                State {
                    initial: true,
                    transitions: Box::new(vec![Transition {
                        read_symbol: 'B',
                        write_symbol: 'B',
                        move_symbol: Move::Left,
                        new_state: "nume".to_owned()
                    }])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_state_2() {
        let state;
        unsafe {
            let state_ptr = test_state(2);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(state, StateType::Accept("okk".to_owned(),))
    }

    #[test]
    #[serial]
    pub fn test_state_3() {
        let state;
        unsafe {
            let state_ptr = test_state(3);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(state, StateType::Reject("nu_ok".to_owned(),))
    }

    #[test]
    #[serial]
    pub fn test_state_4() {
        let state;
        unsafe {
            let state_ptr = test_state(4);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(
            state,
            StateType::State(
                "renume".to_owned(),
                State {
                    initial: false,
                    transitions: Box::new(vec![
                        Transition {
                            read_symbol: 'A',
                            write_symbol: 'A',
                            move_symbol: Move::Neutral,
                            new_state: "nume".to_owned()
                        },
                        Transition {
                            read_symbol: 'H',
                            write_symbol: 'B',
                            move_symbol: Move::Right,
                            new_state: "renume".to_owned()
                        }
                    ])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_state_5() {
        let state;
        unsafe {
            let state_ptr = test_state(5);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(
            state,
            StateType::State(
                "first".to_owned(),
                State {
                    initial: false,
                    transitions: Box::new(vec![Transition {
                        read_symbol: '_',
                        write_symbol: '_',
                        move_symbol: Move::Neutral,
                        new_state: "second".to_owned()
                    }])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_state_6() {
        let state;
        unsafe {
            let state_ptr = test_state(6);
            state = parse_state(state_ptr);
            free_state(state_ptr);
        }
        print!("{:?}", state);
        assert_eq!(
            state,
            StateType::State(
                "q0".to_owned(),
                State {
                    initial: true,
                    transitions: Box::new(vec![Transition {
                        read_symbol: '_',
                        write_symbol: '_',
                        move_symbol: Move::Neutral,
                        new_state: "x.accept".to_owned()
                    }])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_1() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(1);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("comp".to_owned(), MacroType::Complement("and".to_owned()))
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_2() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(2);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro(
                "int".to_owned(),
                MacroType::Intersect(Box::new(vec![
                    "not".to_owned(),
                    "not".to_owned(),
                    "not".to_owned()
                ]))
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_3() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(3);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro(
                "ren".to_owned(),
                MacroType::Reunion(Box::new(vec!["and".to_owned(), "or".to_owned()]))
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_4() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(4);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro(
                "lant".to_owned(),
                MacroType::Chain(Box::new(vec![
                    "not".to_owned(),
                    "or".to_owned(),
                    "move2l".to_owned()
                ]))
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_5() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(5);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro(
                "repetare".to_owned(),
                MacroType::Repeat(5, "not".to_owned())
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_6() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(6);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("move8r".to_owned(), MacroType::Move(Move::Right, 8))
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_7() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(7);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("move2l".to_owned(), MacroType::Move(Move::Left, 2))
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_8() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(8);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro(
                "rescriere".to_owned(),
                MacroType::Override(Move::Left, 5, 'V')
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_9() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(9);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("scriere".to_owned(), MacroType::Place("ABCDE".to_owned()))
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_10() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(10);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("insert8".to_owned(), MacroType::Shift(Move::Right, 8))
        )
    }

    #[test]
    #[serial]
    pub fn test_macro_11() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(11);
            macro_res = parse_automaton(macro_ptr);
            free_automaton(macro_ptr);
        }
        print!("{:?}", macro_res);
        assert_eq!(
            macro_res,
            AutomatonType::Macro("delete19".to_owned(), MacroType::Shift(Move::Left, 19))
        )
    }

    #[test]
    #[serial]
    pub fn test_machine1() {
        let machine_res;
        unsafe {
            let machine_ptr = test_machine(1);
            machine_res = parse_automaton(machine_ptr);
            free_automaton(machine_ptr);
        }
        print!("{:?}", machine_res);
        assert_eq!(
            machine_res,
            AutomatonType::Machine(
                "or".to_owned(),
                Machine {
                    components: Box::new(vec![]),
                    states: Box::new(vec![
                        StateType::State(
                            "q0".to_owned(),
                            State {
                                initial: true,
                                transitions: Box::new(vec![
                                    Transition {
                                        read_symbol: '0',
                                        write_symbol: '0',
                                        move_symbol: Move::Right,
                                        new_state: "q0".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '1',
                                        write_symbol: '1',
                                        move_symbol: Move::Right,
                                        new_state: "q1".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '_',
                                        write_symbol: '_',
                                        move_symbol: Move::Neutral,
                                        new_state: "f".to_owned()
                                    }
                                ])
                            }
                        ),
                        StateType::State(
                            "q1".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![
                                    Transition {
                                        read_symbol: '0',
                                        write_symbol: '1',
                                        move_symbol: Move::Right,
                                        new_state: "q1".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '1',
                                        write_symbol: '1',
                                        move_symbol: Move::Neutral,
                                        new_state: "q1".to_owned(),
                                    },
                                    Transition {
                                        read_symbol: '_',
                                        write_symbol: '_',
                                        move_symbol: Move::Neutral,
                                        new_state: "f".to_owned()
                                    }
                                ])
                            }
                        ),
                        StateType::Accept("f".to_owned())
                    ])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_machine2() {
        let machine_res;
        unsafe {
            let machine_ptr = test_machine(2);
            machine_res = parse_automaton(machine_ptr);
            free_automaton(machine_ptr);
        }
        print!("{:?}", machine_res);
        assert_eq!(
            machine_res,
            AutomatonType::Machine(
                "even".to_owned(),
                Machine {
                    components: Box::new(vec![]),
                    states: Box::new(vec![
                        StateType::State(
                            "even".to_owned(),
                            State {
                                initial: true,
                                transitions: Box::new(vec![
                                    Transition {
                                        read_symbol: '0',
                                        write_symbol: '0',
                                        move_symbol: Move::Right,
                                        new_state: "even".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '1',
                                        write_symbol: '1',
                                        move_symbol: Move::Right,
                                        new_state: "odd".to_owned()
                                    },
                                    Transition {
                                        read_symbol: 'B',
                                        write_symbol: 'B',
                                        move_symbol: Move::Neutral,
                                        new_state: "true".to_owned()
                                    }
                                ])
                            }
                        ),
                        StateType::State(
                            "odd".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![
                                    Transition {
                                        read_symbol: '0',
                                        write_symbol: '0',
                                        move_symbol: Move::Right,
                                        new_state: "odd".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '1',
                                        write_symbol: '1',
                                        move_symbol: Move::Right,
                                        new_state: "even".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '_',
                                        write_symbol: '_',
                                        move_symbol: Move::Neutral,
                                        new_state: "false".to_owned()
                                    }
                                ])
                            }
                        ),
                        StateType::Accept("true".to_owned()),
                        StateType::Reject("false".to_owned())
                    ])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_machine3() {
        let machine_res;
        unsafe {
            let machine_ptr = test_machine(3);
            machine_res = parse_automaton(machine_ptr);
            free_automaton(machine_ptr);
        }
        print!("{:?}", machine_res);
        assert_eq!(
            machine_res,
            AutomatonType::Machine(
                "two_even".to_owned(),
                Machine {
                    components: Box::new(vec![
                        ("even".to_owned(), "first".to_owned()),
                        ("even".to_owned(), "second".to_owned())
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
                                        move_symbol: Move::Right,
                                        new_state: "first.even".to_owned()
                                    },
                                    Transition {
                                        read_symbol: '_',
                                        write_symbol: '_',
                                        move_symbol: Move::Neutral,
                                        new_state: "first.even".to_owned()
                                    }
                                ])
                            }
                        ),
                        StateType::State(
                            "first.true".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![Transition {
                                    read_symbol: '_',
                                    write_symbol: '_',
                                    move_symbol: Move::Right,
                                    new_state: "second.even".to_owned()
                                }])
                            }
                        ),
                        StateType::State(
                            "first.false".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![Transition {
                                    read_symbol: '_',
                                    write_symbol: '_',
                                    move_symbol: Move::Neutral,
                                    new_state: "false".to_owned()
                                }])
                            }
                        ),
                        StateType::State(
                            "second.true".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![Transition {
                                    read_symbol: '_',
                                    write_symbol: '_',
                                    move_symbol: Move::Neutral,
                                    new_state: "true".to_owned()
                                }])
                            }
                        ),
                        StateType::State(
                            "second.false".to_owned(),
                            State {
                                initial: false,
                                transitions: Box::new(vec![Transition {
                                    read_symbol: '_',
                                    write_symbol: '_',
                                    move_symbol: Move::Neutral,
                                    new_state: "false".to_owned()
                                }])
                            }
                        ),
                        StateType::Accept("true".to_owned()),
                        StateType::Reject("false".to_owned())
                    ])
                }
            )
        )
    }

    #[test]
    #[serial]
    pub fn test_program_1() {
        let program_res;
        unsafe {
            let program_ptr = test_program(1);
            program_res = parse_result(program_ptr);
            free_result(program_ptr);
        }
        print!("{:?}", program_res);
        assert_eq!(
            program_res,
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
        )
    }

    #[test]
    #[serial]
    pub fn test_program_2() {
        let program_res;
        unsafe {
            let program_ptr = test_program(2);
            program_res = parse_result(program_ptr);
            free_result(program_ptr);
        }
        print!("{:?}", program_res);
        assert_eq!(
            program_res,
            Err("Error: unexpected keyword - auto at line 2 column 0".to_owned())
        )
    }

    #[test]
    #[serial]
    pub fn test_program_3() {
        let program_res;
        unsafe {
            let program_ptr = test_program(3);
            program_res = parse_result(program_ptr);
            free_result(program_ptr);
        }
        print!("{:?}", program_res);
        assert_eq!(
            program_res,
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
        )
    }
}
