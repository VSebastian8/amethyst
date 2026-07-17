use amethyst::interpreter::Interpreter;

#[test]
pub fn test_simple_automaton() {
    let code = "
        automaton main() {
            initial state first {
                0 / 1, R -> done;
                1 / 0, R -> done;
            }
            accept state done;
        }
        ";
    let mut interp = Interpreter::new();
    interp.load_code(code).unwrap();
    interp.run("main", "007").unwrap();
    assert_eq!("..@|1|0|7|@..", interp.tape());
}

#[test]
pub fn test_abc() {
    let code = "
        {- Validate whether input has form a^nb^nc^n
            e.g. ..@|A|A|B|B|C|C|@.. => accept
                 ..@|A|A|A|B|C|C|@.. => reject
        -}
        automaton validator() {
            -- At start of input
            initial state q0 {
                A / X, R -> q1; -- consume first A
                _ / _, N -> no_more_a;
            }
            -- Just consumed 1 A
            state q1 {
                A / A, R -> q1;
                Y / Y, R -> q1;
                B / Y, R -> q2; -- consume first B
                _ / _, N -> false;
            }
            state q2 {
                B / B, R -> q2;
                Z / Z, R -> q2;
                C / Z, N -> q3;
                _ / _, N -> false;
            }
            -- Find first A
            state q3 {
                X / X, R -> q0;
                _ / _, L -> q3;
            }
            -- Make sure no other Bs or Cs exist
            state no_more_a {
                B / B, N -> false;
                C / C, N -> false;
                @ / @, L -> clean;
                _ / _, R -> no_more_a;
            }
            state clean {
                X / A, L -> clean;
                Y / B, L -> clean;
                Z / C, L -> clean;
                @ / @, R -> true;

            }
            accept state true;
            reject state false;
        }
        ";
    let mut interp = Interpreter::new();
    interp.load_code(code).unwrap();
    interp.run("validator", "").unwrap();
    assert_eq!("validator.true", interp.state);
    interp.run("validator", "AABBCC").unwrap();
    assert_eq!("validator.true", interp.state);
    interp.run("validator", "AABCC").unwrap();
    assert_eq!("validator.false", interp.state);
    interp.run("validator", "AAAAABBBBBCCCCC").unwrap();
    assert_eq!("validator.true", interp.state);
}
