extern "C" {
    fn initialize_haskell();
    fn exit_haskell();
    // Test functions
    fn test_transition(n: i32) -> *mut i32;
    fn test_state(n: i32) -> *mut i32;
}

use amethyst::parser::{parse_state, parse_transition};
use serial_test::serial;

#[cfg(test)]
mod tests {
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
        unsafe {
            let transition_ptr = test_transition(1);
            let transition = parse_transition(transition_ptr);
            println!("{:?}", transition);
        }
    }

    #[test]
    #[serial]
    pub fn test_transition_2() {
        unsafe {
            let transition_ptr = test_transition(2);
            let transition = parse_transition(transition_ptr);
            println!("{:?}", transition);
        }
    }

    #[test]
    #[serial]
    pub fn test_state_1() {
        unsafe {
            let state_ptr = test_state(1);
            let state = parse_state(state_ptr);
            println!("{:?}", state);
        }
    }

    #[test]
    #[serial]
    pub fn test_state_2() {
        unsafe {
            let state_ptr = test_state(2);
            let state = parse_state(state_ptr);
            println!("{:?}", state);
        }
    }

    #[test]
    #[serial]
    pub fn test_state_3() {
        unsafe {
            let state_ptr = test_state(3);
            let state = parse_state(state_ptr);
            println!("{:?}", state);
        }
    }

    #[test]
    #[serial]
    pub fn test_state_4() {
        unsafe {
            let state_ptr = test_state(4);
            let state = parse_state(state_ptr);
            println!("{:?}", state);
        }
    }
}
