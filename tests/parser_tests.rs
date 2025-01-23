extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    // Always free memory after using it
    fn free_transition(transition_ptr: *mut i32);
    fn free_state(state: *mut i32);
    // Test functions
    fn test_transition(n: i32) -> *mut i32;
    fn test_state(n: i32) -> *mut i32;
    fn test_macro(n: i32) -> *mut i32;
}

use amethyst::parser::{parse_automata, parse_state, parse_transition};
use serial_test::serial;

#[cfg(test)]
mod tests {
    use amethyst::syntax::{Move, State, StateType, Transition};

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
    pub fn test_macro_1() {
        let macro_res;
        unsafe {
            let macro_ptr = test_macro(1);
            macro_res = parse_automata(macro_ptr);
            // free_automata(macro_ptr);
        }
        print!("{:?}", macro_res)
    }
}
