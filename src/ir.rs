use crate::ast::*;
use std::collections::{HashMap, HashSet};

// Component Removal IR
#[derive(Debug)]
pub struct IR {
    pub initial_state: String,
    pub transition_states: HashSet<String>,
    pub accept_states: HashSet<String>,
    pub reject_states: HashSet<String>,
    pub transitions: HashMap<String, HashMap<char, (char, Move, String)>>,
}

// TODO: better states/components errors
impl IR {
    pub fn new() -> Self {
        IR {
            initial_state: "".to_string(),
            transition_states: HashSet::new(),
            accept_states: HashSet::new(),
            reject_states: HashSet::new(),
            transitions: HashMap::new(),
        }
    }

    // Adding a transition, checking the new state exists
    fn add_transition(
        &mut self,
        prefix: &String,
        from_state: &String,
        comps_input: &HashMap<String, String>,
        t: &Transition,
    ) -> Result<(), String> {
        let state = match &t.state {
            (name, Some(comp)) => {
                format!(
                    "{}.{}.{}",
                    prefix,
                    comp,
                    if name == "input" {
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
            return Err(format!("Unknown state {}", state));
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
        Ok(())
    }

    // ensure state does not appear before
    fn unique_state(&self, state: String) -> Result<String, String> {
        if self.accept_states.contains(&state)
            || self.reject_states.contains(&state)
            || self.transition_states.contains(&state)
        {
            Err("State already defined".to_string())
        } else {
            Ok(state)
        }
    }

    // without transitions or rewriting component states
    fn add_shallow_state(&mut self, prefix: &String, state: &State) -> Result<(), String> {
        match state {
            State::Accept(name) => {
                self.accept_states
                    .insert(self.unique_state(format!("{}.{}", prefix, name))?);
            }
            State::Reject(name) => {
                self.reject_states
                    .insert(self.unique_state(format!("{}.{}", prefix, name))?);
            }
            State::State(name, component, _, _) => match component {
                None => {
                    self.transition_states
                        .insert(self.unique_state(format!("{}.{}", prefix, name))?);
                }
                Some(_comp) => {
                    // Done in full state
                }
            },
        };
        Ok(())
    }

    // with transitions
    fn add_full_state(
        &mut self,
        prefix: &String,
        state: &State,
        comps_input: &HashMap<String, String>,
        comps_output: &HashMap<String, Vec<(String, bool)>>,
    ) -> Result<(), String> {
        // println!("Adding full state {:?}", state);
        match state {
            State::State(name, component, _initial, transitions) => match component {
                None => transitions
                    .iter()
                    .map(|t| self.add_transition(prefix, name, comps_input, t))
                    .collect::<Result<(), String>>(),
                Some(comp) => {
                    // Check that component exists
                    if !comps_input.contains_key(comp) {
                        return Err(format!("Could not find component {}", comp));
                    }
                    // Check that state doesn't already exist (unless final)
                    if self
                        .transition_states
                        .contains(&format!("{}.{}.{}", prefix, comp, name))
                    {
                        return Err("State already defined".to_string());
                    }
                    // Handle special syntax for component states
                    match name.as_str() {
                        "accept" | "reject" | "output" => {
                            // Rewrite final states of the component
                            comps_output[comp]
                                .iter()
                                .map(|(st, acc)| {
                                    if name == "accept" && !*acc {
                                        return Ok(());
                                    }
                                    if name == "reject" && *acc {
                                        return Ok(());
                                    }
                                    if *acc {
                                        self.accept_states
                                            .remove(&format!("{}.{}.{}", prefix, comp, st));
                                    } else {
                                        self.reject_states
                                            .remove(&format!("{}.{}.{}", prefix, comp, st));
                                    }
                                    self.transition_states
                                        .insert(format!("{}.{}.{}", prefix, comp, st));
                                    transitions
                                        .iter()
                                        .map(|t| {
                                            self.add_transition(
                                                prefix,
                                                &format!("{}.{}", comp, st),
                                                comps_input,
                                                t,
                                            )
                                        })
                                        .collect::<Result<(), String>>()
                                })
                                .collect::<Result<(), String>>()
                        }
                        _ => {
                            // Only rewrite final states
                            if comps_output[comp].iter().all(|(st, _)| st != name) {
                                return Err(
                                    "Can only rewrite final states from components".to_string()
                                );
                            }
                            self.accept_states
                                .remove(&format!("{}.{}.{}", prefix, comp, name));
                            self.reject_states
                                .remove(&format!("{}.{}.{}", prefix, comp, name));
                            self.transition_states
                                .insert(format!("{}.{}.{}", prefix, comp, name));
                            transitions
                                .iter()
                                .map(|t| {
                                    self.add_transition(
                                        prefix,
                                        &format!("{}.{}", comp, name),
                                        comps_input,
                                        t,
                                    )
                                })
                                .collect::<Result<(), String>>()
                        }
                    }
                }
            },
            _ => Ok(()),
        }
    }

    // Returns initial state and final states
    fn add_automaton(
        &mut self,
        automata: &HashMap<String, &Automaton>,
        visited: &mut HashSet<String>,
        prefix: &String,
        name: &String,
    ) -> Result<(String, Vec<(String, bool)>), String> {
        if !automata.contains_key(name) {
            return Err("Automaton does not exist".to_string());
        }
        if visited.contains(name) {
            return Err("Component cycles are not allowed".to_string());
        }
        visited.insert(name.clone());
        // Recursively add components
        let mut comps_input: HashMap<String, String> = HashMap::new();
        let mut comps_output: HashMap<String, Vec<(String, bool)>> = HashMap::new();
        automata[name]
            .components
            .iter()
            .map(|(auto, comp)| {
                let (comp_input, mut comp_outputs) =
                    self.add_automaton(automata, visited, &format!("{}.{}", prefix, comp), auto)?;
                comps_input.insert(comp.clone(), comp_input);
                comps_output
                    .entry(comp.clone())
                    .or_insert(Vec::new())
                    .append(&mut comp_outputs);
                Ok(())
            })
            .collect::<Result<(), String>>()?;
        visited.remove(name);
        // Add shallow states, then full states
        automata[name]
            .states
            .iter()
            .map(|state| self.add_shallow_state(prefix, state))
            .collect::<Result<(), String>>()?;
        automata[name]
            .states
            .iter()
            .map(|state| self.add_full_state(prefix, state, &comps_input, &comps_output))
            .collect::<Result<(), String>>()?;
        // Collect final states
        let final_states = automata[name]
            .states
            .iter()
            .flat_map(|state| match state {
                State::Accept(name) => Some((name.clone(), true)),
                State::Reject(name) => Some((name.clone(), false)),
                _ => None,
            })
            .collect();
        // Check initial state validity
        let mut initial_state: Option<String> = None;
        automata[name]
            .states
            .iter()
            .map(|state| match state {
                State::State(name, None, initial, _) => {
                    if *initial {
                        if initial_state.is_some() {
                            Err("Can't have multiple initial states".to_string())
                        } else {
                            initial_state = Some(name.clone());
                            Ok(())
                        }
                    } else {
                        Ok(())
                    }
                }
                State::State(_, Some(_), initial, _) => {
                    if *initial {
                        Err("Component state can't be initial".to_string())
                    } else {
                        Ok(())
                    }
                }
                _ => Ok(()),
            })
            .collect::<Result<(), String>>()?;
        match initial_state {
            None => Err("No initial state".to_string()),
            Some(init) => Ok((init, final_states)),
        }
    }
}

// Add automatic sink state for full coverage
fn add_sink_states(program: &mut Vec<Automaton>) {
    program.iter_mut().for_each(|automaton| {
        let mut sink = false;
        automaton.states.iter_mut().for_each(|state| match state {
            State::State(_, _, _, transitions) => {
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
            automaton.states.push(State::Reject("sink".to_string()));
        }
    });
}

pub fn remove_components(mut program: Vec<Automaton>) -> Result<HashMap<String, IR>, String> {
    add_sink_states(&mut program);
    let mut visited = HashSet::new();
    let automata: HashMap<_, _> = program
        .iter()
        .map(|auto| (auto.name.clone(), auto))
        .collect();
    program
        .iter()
        .map(|auto| {
            let mut ir = IR::new();
            let (initial, _) = ir.add_automaton(&automata, &mut visited, &auto.name, &auto.name)?;
            ir.initial_state = format!("{}.{}", auto.name, initial);
            Ok((auto.name.clone(), ir))
        })
        .collect()
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
                State::State(
                    "first".to_string(),
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
                State::Accept("good".to_string()),
                State::Reject("bad".to_string()),
            ],
        }];
        let result = remove_components(program).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result["main"].initial_state, "main.first".to_string());
        assert!(result["main"]
            .transition_states
            .contains(&"main.first".to_string()));
        assert!(result["main"]
            .accept_states
            .contains(&"main.good".to_string()));
        assert!(result["main"]
            .reject_states
            .contains(&"main.bad".to_string()));
        assert!(result["main"].transitions.contains_key("main.first"));
        assert_eq!(
            result["main"].transitions["main.first"],
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
                    State::State(
                        "q0".to_string(),
                        None,
                        true,
                        vec![Transition {
                            read: '_',
                            write: '_',
                            mov: Move::R,
                            state: ("q1".to_string(), None),
                        }],
                    ),
                    State::Accept("q1".to_string()),
                ],
            },
            Automaton {
                name: "add".to_string(),
                components: vec![("move".to_string(), "m".to_string())],
                states: vec![
                    State::State(
                        "first".to_string(),
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
                    State::State(
                        "q1".to_string(),
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
                    State::Accept("done".to_string()),
                    State::Reject("ups".to_string()),
                ],
            },
            Automaton {
                name: "main".to_string(),
                components: vec![
                    ("add".to_string(), "a1".to_string()),
                    ("add".to_string(), "a2".to_string()),
                ],
                states: vec![
                    State::State(
                        "first".to_string(),
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
                    State::State(
                        "output".to_string(),
                        Some("a1".to_string()),
                        false,
                        vec![Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: ("finally".to_string(), None),
                        }],
                    ),
                    State::State(
                        "accept".to_string(),
                        Some("a2".to_string()),
                        false,
                        vec![Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: ("finally".to_string(), None),
                        }],
                    ),
                    State::State(
                        "reject".to_string(),
                        Some("a2".to_string()),
                        false,
                        vec![Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: ("double_ups".to_string(), None),
                        }],
                    ),
                    State::Accept("finally".to_string()),
                    State::Reject("double_ups".to_string()),
                ],
            },
        ];
        let result = remove_components(program).unwrap();
        assert_eq!(result.len(), 3);
        let main_ir = &result["main"];
        assert_eq!(main_ir.initial_state, "main.first".to_string());
        assert_eq!(
            main_ir.accept_states,
            HashSet::from(["main.finally".to_string()])
        );
        assert_eq!(
            main_ir.reject_states,
            HashSet::from(["main.double_ups".to_string()])
        );
        assert_eq!(
            main_ir.transition_states,
            HashSet::from([
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
            main_ir.transitions,
            HashMap::from([
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
