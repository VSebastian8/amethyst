use crate::info::Error;
use crate::{ast::*, info::Info};
use std::collections::{HashMap, HashSet};

// Component Removal IR
#[derive(Debug, Clone)]
pub struct IR {
    pub initial_states: HashMap<String, String>,
    pub transition_states: HashSet<String>,
    pub accept_states: HashSet<String>,
    pub reject_states: HashSet<String>,
    pub transitions: HashMap<String, HashMap<char, (char, Move, String)>>,
    pub errors: Vec<Error>,
}

impl IR {
    pub fn new() -> Self {
        IR {
            initial_states: HashMap::new(),
            transition_states: HashSet::new(),
            accept_states: HashSet::new(),
            reject_states: HashSet::new(),
            transitions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    // Adding a transition, checking the new state exists
    fn add_transition(
        &mut self,
        prefix: &String,
        from_state: &String,
        comps_input: &HashMap<String, String>,
        t: &Transition,
    ) {
        let state = match &t.state {
            (name, Some(comp)) => {
                format!(
                    "{}.{}.{}",
                    prefix,
                    comp,
                    if name == "input" {
                        if !comps_input.contains_key(comp) {
                            self.errors.push(Error::Unknown(
                                "component alias".to_string(),
                                comp.clone(),
                                Info {
                                    line: 0,
                                    from: 0,
                                    to: 0,
                                },
                            ));
                            return;
                        }
                        &comps_input[comp]
                    } else {
                        name
                    }
                )
            }
            (name, None) => format!("{}.{}", prefix, name),
        };
        // println!("Adding transition {:?} with state {:?}", t, state);
        if !self.transition_states.contains(&state)
            && !self.accept_states.contains(&state)
            && !self.reject_states.contains(&state)
        {
            self.errors.push(Error::Unknown(
                "state".to_string(),
                state,
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ));
            return;
        }
        let from = format!("{}.{}", prefix, from_state);
        let state_trans = self.transitions.entry(from).or_insert(HashMap::new());
        if state_trans.contains_key(&t.read) {
            println!(
                "WARNING: Unreachable transition, read symbol {} already covered",
                t.read
            );
        } else {
            state_trans.insert(t.read, (t.write, t.mov.clone(), state));
        }
    }

    // ensure state does not appear before
    fn unique_state(&mut self, state: &String) {
        if self.accept_states.contains(state)
            || self.reject_states.contains(state)
            || self.transition_states.contains(state)
        {
            self.errors.push(Error::Defined(
                "State".to_string(),
                state.clone(),
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ))
        }
    }

    // without transitions or rewriting component states
    fn add_shallow_state(&mut self, prefix: &String, state: &State) {
        // Insert component states in full state
        match &state.typ {
            StateType::State(component, _, _) if component.is_some() => return,
            _ => {}
        }
        let name = state.name.clone();
        let state_name = format!("{}.{}", prefix, name);
        self.unique_state(&state_name);
        match &state.typ {
            StateType::Accept => {
                self.accept_states.insert(state_name);
            }
            StateType::Reject => {
                self.reject_states.insert(state_name);
            }
            StateType::State(component, _, _) => match component {
                None => {
                    self.transition_states.insert(state_name);
                }
                Some(_comp) => {}
            },
        };
    }

    fn add_blueprint_state(
        &mut self,
        prefix: &String,
        name: &String,
        comp: &String,
        transitions: &Vec<Transition>,
        comps_input: &HashMap<String, String>,
        comps_output: &HashMap<String, Vec<(String, bool)>>,
    ) {
        // Check that component exists
        if !comps_input.contains_key(comp) {
            self.errors.push(Error::Unknown(
                "component alias".to_string(),
                comp.clone(),
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ));
            return;
        }
        // Check that state doesn't already exist (unless final)
        let state_name = format!("{}.{}.{}", prefix, comp, name);
        if self.transition_states.contains(&state_name) {
            self.errors.push(Error::Defined(
                "State".to_string(),
                state_name.clone(),
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ));
            return;
        }
        // Handle special syntax for component states
        match name.as_str() {
            "accept" | "reject" | "output" => {
                // Rewrite final states of the component
                comps_output[comp].iter().for_each(|(st, acc)| {
                    // Skip accepting/rejecting states when the component's sign differs
                    if (name == "accept" && !*acc) || (name == "reject" && *acc) {
                        return;
                    }
                    let rewritten_state = format!("{}.{}.{}", prefix, comp, st);
                    if *acc {
                        self.accept_states.remove(&rewritten_state);
                    } else {
                        self.reject_states.remove(&rewritten_state);
                    }
                    self.transition_states.insert(rewritten_state);
                    transitions.iter().for_each(|t| {
                        self.add_transition(prefix, &format!("{}.{}", comp, st), comps_input, t)
                    });
                })
            }
            _ => {
                // Only rewrite final states
                if comps_output[comp].iter().all(|(st, _)| st != name) {
                    self.errors.push(Error::NotAllowed(
                        "Rewriting non-final blueprint states".to_string(),
                        Info {
                            line: 0,
                            from: 0,
                            to: 0,
                        },
                    ));
                    return;
                }
                self.accept_states.remove(&state_name);
                self.reject_states.remove(&state_name);
                self.transition_states.insert(state_name);
                transitions.iter().for_each(|t| {
                    self.add_transition(prefix, &format!("{}.{}", comp, name), comps_input, t)
                });
            }
        }
    }

    // with transitions
    fn add_full_state(
        &mut self,
        prefix: &String,
        state: &State,
        comps_input: &HashMap<String, String>,
        comps_output: &HashMap<String, Vec<(String, bool)>>,
    ) {
        let name = state.name.clone();
        if let StateType::State(component, _initial, transitions) = &state.typ {
            match component {
                None => {
                    transitions
                        .iter()
                        .for_each(|t| self.add_transition(prefix, &name, comps_input, t));
                }
                Some(comp) => self.add_blueprint_state(
                    prefix,
                    &name,
                    comp,
                    transitions,
                    comps_input,
                    comps_output,
                ),
            }
        }
    }

    // It exists and there are no component cycles
    fn validate_automaton(
        &mut self,
        name: &String,
        automata: &HashMap<String, &Automaton>,
        visited: &HashSet<String>,
    ) -> Option<()> {
        if !automata.contains_key(name) {
            self.errors.push(Error::Unknown(
                "automaton".to_string(),
                name.clone(),
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ));
            return None;
        }
        if visited.contains(name) {
            self.errors.push(Error::Cycle(
                "component".to_string(),
                name.clone(),
                Info {
                    line: 0,
                    from: 0,
                    to: 0,
                },
            ));
            return None;
        }
        Some(())
    }

    // Returns initial state and final states
    fn add_automaton(
        &mut self,
        automata: &HashMap<String, &Automaton>,
        visited: &mut HashSet<String>,
        prefix: &String,
        name: &String,
    ) -> Option<(String, Vec<(String, bool)>)> {
        self.validate_automaton(name, automata, visited)?;
        visited.insert(name.clone());
        // Recursively add components
        let mut comps_input: HashMap<String, String> = HashMap::new();
        let mut comps_output: HashMap<String, Vec<(String, bool)>> = HashMap::new();
        automata[name].components.iter().for_each(|(auto, comp)| {
            self.add_automaton(automata, visited, &format!("{}.{}", prefix, comp), auto)
                .into_iter()
                .for_each(|(comp_input, mut comp_outputs)| {
                    comps_input.insert(comp.clone(), comp_input);
                    comps_output
                        .entry(comp.clone())
                        .or_insert(Vec::new())
                        .append(&mut comp_outputs);
                })
        });
        visited.remove(name);
        // Add shallow states, then full states
        for state in automata[name].states.iter() {
            self.add_shallow_state(prefix, state);
        }
        for state in automata[name].states.iter() {
            self.add_full_state(prefix, state, &comps_input, &comps_output);
        }
        // Collect final states
        let final_states = automata[name]
            .states
            .iter()
            .flat_map(|state| match state.typ {
                StateType::Accept => Some((state.name.clone(), true)),
                StateType::Reject => Some((state.name.clone(), false)),
                _ => None,
            })
            .collect();
        // Check initial state validity
        let mut initial_state: Option<String> = None;
        for state in automata[name].states.iter() {
            if let StateType::State(comp, true, _) = &state.typ {
                if comp.is_none() {
                    if initial_state.is_some() {
                        self.errors.push(Error::NotAllowed(
                            "Having multiple initial states".to_string(),
                            Info {
                                line: 0,
                                from: 0,
                                to: 0,
                            },
                        ));
                    } else {
                        initial_state = Some(state.name.clone());
                    }
                } else {
                    self.errors.push(Error::NotAllowed(
                        "Marking component state as initial".to_string(),
                        Info {
                            line: 0,
                            from: 0,
                            to: 0,
                        },
                    ));
                }
            }
        }

        (initial_state?, final_states).into()
    }
}

// Add automatic sink state for full coverage
fn add_sink_states(program: &mut Vec<Automaton>) {
    program.iter_mut().for_each(|automaton| {
        let mut sink = false;
        automaton
            .states
            .iter_mut()
            .for_each(|state| match &mut state.typ {
                StateType::State(_, _, transitions) => {
                    if !transitions.iter().any(|t| t.read == '_') {
                        transitions.push(Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: ("sink".to_string(), None),
                        });
                        sink = true;
                    }
                }
                _ => {}
            });
        if sink {
            automaton.states.push(State {
                name: "sink".to_string(),
                typ: StateType::Reject,
                desc: "generated sink state".to_string(),
            });
        }
    });
}

pub fn remove_components(mut program: Vec<Automaton>) -> IR {
    add_sink_states(&mut program);
    let mut visited = HashSet::new();
    let automata: HashMap<_, _> = program
        .iter()
        .map(|auto| (auto.name.clone(), auto))
        .collect();
    let mut ir = IR::new();
    for auto in program.iter() {
        if let Some((initial, _)) =
            ir.add_automaton(&automata, &mut visited, &auto.name, &auto.name)
        {
            ir.initial_states
                .insert(auto.name.clone(), format!("{}.{}", auto.name, initial));
        }
    }
    ir
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_flat_automaton() {
        let program = vec![Automaton {
            name: "main".to_string(),
            components: Vec::new(),
            states: vec![
                State {
                    name: "first".to_string(),
                    typ: StateType::State(
                        None,
                        true,
                        vec![
                            Transition {
                                read: '0',
                                write: 'A',
                                mov: Move::L,
                                state: ("good".to_string(), None),
                            },
                            Transition {
                                read: '_',
                                write: 'B',
                                mov: Move::R,
                                state: ("bad".to_string(), None),
                            },
                        ],
                    ),
                    desc: String::new(),
                },
                State {
                    name: "good".to_string(),
                    typ: StateType::Accept,
                    desc: "accepting state".to_string(),
                },
                State {
                    name: "bad".to_string(),
                    typ: StateType::Reject,
                    desc: "rejecting state".to_string(),
                },
            ],
            desc: String::new(),
        }];
        let result = remove_components(program);
        assert_eq!(result.initial_states.len(), 1);
        assert_eq!(result.initial_states["main"], "main.first".to_string());
        assert!(result.transition_states.contains(&"main.first".to_string()));
        assert!(result.accept_states.contains(&"main.good".to_string()));
        assert!(result.reject_states.contains(&"main.bad".to_string()));
        assert!(result.transitions.contains_key("main.first"));
        assert_eq!(
            result.transitions["main.first"],
            HashMap::from([
                ('0', ('A', Move::L, "main.good".to_string())),
                ('_', ('B', Move::R, "main.bad".to_string()))
            ])
        );
    }

    #[test]
    pub fn test_nested_automaton() {
        let program = vec![
            Automaton {
                name: "move".to_string(),
                components: Vec::new(),
                states: vec![
                    State {
                        name: "q0".to_string(),
                        typ: StateType::State(
                            None,
                            true,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::R,
                                state: ("q1".to_string(), None),
                            }],
                        ),
                        desc: "simple state".to_string(),
                    },
                    State {
                        name: "q1".to_string(),
                        typ: StateType::Accept,
                        desc: "final state".to_string(),
                    },
                ],
                desc: "move\none\ncell".to_string(),
            },
            Automaton {
                name: "add".to_string(),
                components: vec![("move".to_string(), "m".to_string())],
                states: vec![
                    State {
                        name: "first".to_string(),
                        typ: StateType::State(
                            None,
                            true,
                            vec![
                                Transition {
                                    read: '1',
                                    write: '0',
                                    mov: Move::N,
                                    state: ("q0".to_string(), Some("m".to_string())),
                                },
                                Transition {
                                    read: '0',
                                    write: '1',
                                    mov: Move::N,
                                    state: ("input".to_string(), Some("m".to_string())),
                                },
                            ],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: "q1".to_string(),
                        typ: StateType::State(
                            Some("m".to_string()),
                            false,
                            vec![
                                Transition {
                                    read: 'A',
                                    write: '_',
                                    mov: Move::N,
                                    state: ("done".to_string(), None),
                                },
                                Transition {
                                    read: '_',
                                    write: 'B',
                                    mov: Move::N,
                                    state: ("ups".to_string(), None),
                                },
                            ],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: "done".to_string(),
                        typ: StateType::Accept,
                        desc: String::new(),
                    },
                    State {
                        name: "ups".to_string(),
                        typ: StateType::Reject,
                        desc: "upsie".to_string(),
                    },
                ],
                desc: "some complicated machine".to_string(),
            },
            Automaton {
                name: "main".to_string(),
                components: vec![
                    ("add".to_string(), "a1".to_string()),
                    ("add".to_string(), "a2".to_string()),
                ],
                states: vec![
                    State {
                        name: "first".to_string(),
                        typ: StateType::State(
                            None,
                            true,
                            vec![
                                Transition {
                                    read: '&',
                                    write: '@',
                                    mov: Move::L,
                                    state: ("input".to_string(), Some("a1".to_string())),
                                },
                                Transition {
                                    read: '_',
                                    write: '2',
                                    mov: Move::N,
                                    state: ("first".to_string(), Some("a2".to_string())),
                                },
                            ],
                        ),
                        desc: "this state is pretty cool huh".to_string(),
                    },
                    State {
                        name: "output".to_string(),
                        typ: StateType::State(
                            Some("a1".to_string()),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: ("finally".to_string(), None),
                            }],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: "accept".to_string(),
                        typ: StateType::State(
                            Some("a2".to_string()),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: ("finally".to_string(), None),
                            }],
                        ),
                        desc: "all accepting state of component a2".to_string(),
                    },
                    State {
                        name: "reject".to_string(),
                        typ: StateType::State(
                            Some("a2".to_string()),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: ("double_ups".to_string(), None),
                            }],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: "finally".to_string(),
                        typ: StateType::Accept,
                        desc: String::new(),
                    },
                    State {
                        name: "double_ups".to_string(),
                        typ: StateType::Reject,
                        desc: "you really messed up".to_string(),
                    },
                ],
                desc: "turing machines are cool".to_string(),
            },
        ];
        let ir = remove_components(program);
        assert_eq!(ir.initial_states.len(), 3);
        assert_eq!(
            ir.initial_states,
            HashMap::from([
                ("move".to_string(), "move.q0".to_string()),
                ("add".to_string(), "add.first".to_string()),
                ("main".to_string(), "main.first".to_string()),
            ])
        );
        assert_eq!(
            ir.accept_states,
            HashSet::from([
                "move.q1".to_string(),
                "add.done".to_string(),
                "main.finally".to_string(),
                "main.finally".to_string(),
                "main.finally".to_string()
            ])
        );
        assert_eq!(
            ir.reject_states,
            HashSet::from([
                "add.ups".to_string(),
                "add.sink".to_string(),
                "main.double_ups".to_string()
            ])
        );
        assert_eq!(
            ir.transition_states,
            HashSet::from([
                "move.q0".to_string(),
                "add.first".to_string(),
                "add.m.q0".to_string(),
                "add.m.q1".to_string(),
                "main.first".to_string(),
                "main.a1.first".to_string(),
                "main.a1.done".to_string(),
                "main.a1.ups".to_string(),
                "main.a1.sink".to_string(),
                "main.a1.m.q0".to_string(),
                "main.a1.m.q1".to_string(),
                "main.a2.first".to_string(),
                "main.a2.done".to_string(),
                "main.a2.ups".to_string(),
                "main.a2.sink".to_string(),
                "main.a2.m.q0".to_string(),
                "main.a2.m.q1".to_string()
            ])
        );
        assert_eq!(
            ir.transitions,
            HashMap::from([
                (
                    "move.q0".to_string(),
                    HashMap::from([('_', ('_', Move::R, "move.q1".to_string()))])
                ),
                (
                    "add.first".to_string(),
                    HashMap::from([
                        ('1', ('0', Move::N, "add.m.q0".to_string())),
                        ('0', ('1', Move::N, "add.m.q0".to_string())),
                        ('_', ('_', Move::N, "add.sink".to_string()))
                    ])
                ),
                (
                    "add.m.q0".to_string(),
                    HashMap::from([('_', ('_', Move::R, "add.m.q1".to_string()))])
                ),
                (
                    "add.m.q1".to_string(),
                    HashMap::from([
                        ('A', ('_', Move::N, "add.done".to_string())),
                        ('_', ('B', Move::N, "add.ups".to_string()))
                    ])
                ),
                (
                    "main.a1.m.q0".to_string(),
                    HashMap::from([('_', ('_', Move::R, "main.a1.m.q1".to_string()))])
                ),
                (
                    "main.a1.m.q1".to_string(),
                    HashMap::from([
                        ('A', ('_', Move::N, "main.a1.done".to_string())),
                        ('_', ('B', Move::N, "main.a1.ups".to_string()))
                    ])
                ),
                (
                    "main.a1.first".to_string(),
                    HashMap::from([
                        ('1', ('0', Move::N, "main.a1.m.q0".to_string())),
                        ('0', ('1', Move::N, "main.a1.m.q0".to_string())),
                        ('_', ('_', Move::N, "main.a1.sink".to_string()))
                    ])
                ),
                (
                    "main.a2.m.q0".to_string(),
                    HashMap::from([('_', ('_', Move::R, "main.a2.m.q1".to_string()))])
                ),
                (
                    "main.a2.m.q1".to_string(),
                    HashMap::from([
                        ('A', ('_', Move::N, "main.a2.done".to_string())),
                        ('_', ('B', Move::N, "main.a2.ups".to_string()))
                    ])
                ),
                (
                    "main.a2.first".to_string(),
                    HashMap::from([
                        ('1', ('0', Move::N, "main.a2.m.q0".to_string())),
                        ('0', ('1', Move::N, "main.a2.m.q0".to_string())),
                        ('_', ('_', Move::N, "main.a2.sink".to_string()))
                    ])
                ),
                (
                    "main.first".to_string(),
                    HashMap::from([
                        ('&', ('@', Move::L, "main.a1.first".to_string())),
                        ('_', ('2', Move::N, "main.a2.first".to_string()))
                    ])
                ),
                (
                    "main.a1.done".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.finally".to_string())),])
                ),
                (
                    "main.a1.ups".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.finally".to_string())),])
                ),
                (
                    "main.a1.sink".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.finally".to_string())),])
                ),
                (
                    "main.a2.done".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.finally".to_string())),])
                ),
                (
                    "main.a2.ups".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.double_ups".to_string())),])
                ),
                (
                    "main.a2.sink".to_string(),
                    HashMap::from([('_', ('_', Move::N, "main.double_ups".to_string())),])
                )
            ])
        );
    }
}
