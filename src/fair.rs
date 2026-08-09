use crate::ast::*;
use crate::info::*;
use std::collections::{HashMap, HashSet};

// Flattened Automata Intermediate Representation (component desugaring)
#[derive(Debug, Clone)]
pub struct FAIR {
    pub initial_states: HashMap<String, String>,
    pub transition_states: HashSet<String>,
    pub accept_states: HashSet<String>,
    pub reject_states: HashSet<String>,
    pub transitions: HashMap<String, HashMap<char, (char, Move, String)>>,
    pub errors: Vec<Error>,
}

impl FAIR {
    pub fn new() -> Self {
        FAIR {
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
        let StringInfo { name, info } = &t.state.0;
        let state = match &t.state.1 {
            Some(StringInfo {
                name: comp,
                info: cinfo,
            }) => {
                format!(
                    "{}{}.{}",
                    prefix,
                    comp,
                    if name == "input" {
                        if !comps_input.contains_key(comp) {
                            self.errors.push(Error::Unknown {
                                typ: "component alias".to_string(),
                                found: comp.clone(),
                                info: cinfo.clone(),
                            });
                            return;
                        }
                        &comps_input[comp]
                    } else {
                        name
                    }
                )
            }
            None => format!("{}{}", prefix, name),
        };
        // println!("Adding transition {:?} with state {:?}", t, state);
        if !self.transition_states.contains(&state)
            && !self.accept_states.contains(&state)
            && !self.reject_states.contains(&state)
        {
            self.errors.push(Error::Unknown {
                typ: "state".to_string(),
                found: state,
                info: info.clone(),
            });
            return;
        }
        let from = format!("{}{}", prefix, from_state);
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
    fn unique_state(&mut self, prefix: &String, state: &StringInfo) {
        let name = format!("{}{}", prefix, state.name);
        if self.accept_states.contains(&name)
            || self.reject_states.contains(&name)
            || self.transition_states.contains(&name)
        {
            self.errors.push(Error::Defined {
                typ: "State".to_string(),
                name,
                info: state.info.clone(),
            })
        }
    }

    // without transitions or rewriting component states
    fn add_shallow_state(&mut self, prefix: &String, state: &State) {
        // Insert component states in full state
        match &state.typ {
            StateType::State(component, _, _) if component.is_some() => return,
            _ => {}
        }
        self.unique_state(prefix, &state.name);
        let StringInfo { name, .. } = state.name.clone();
        let state_name = format!("{}{}", prefix, name);
        match &state.typ {
            StateType::Accept => {
                self.accept_states.insert(state_name);
            }
            StateType::Reject => {
                self.reject_states.insert(state_name);
            }
            StateType::State(None, _, _) => {
                self.transition_states.insert(state_name);
            }
            _ => {}
        };
    }

    fn add_blueprint_state(
        &mut self,
        prefix: &String,
        state: &StringInfo,
        blueprint: &StringInfo,
        transitions: &Vec<Transition>,
        comps_input: &HashMap<String, String>,
        comps_output: &HashMap<String, Vec<(String, bool)>>,
    ) {
        let StringInfo { name, info } = state;
        let StringInfo {
            name: comp,
            info: cinfo,
        } = blueprint;
        // Check that component exists
        if !comps_input.contains_key(comp) {
            self.errors.push(Error::Unknown {
                typ: "component alias".to_string(),
                found: comp.clone(),
                info: cinfo.clone(),
            });
            return;
        }
        // Check that state doesn't already exist (unless final) TODO: Think about this?
        let state_name = format!("{}{}.{}", prefix, comp, name);
        if self.transition_states.contains(&state_name) {
            self.errors.push(Error::Defined {
                typ: "State".to_string(),
                name: state_name.clone(),
                info: info.clone(),
            });
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
                    let rewritten_state = format!("{}{}.{}", prefix, comp, st);
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
                    self.errors.push(Error::NotAllowed {
                        reason: "Rewriting non-final blueprint states".to_string(),
                        info: info.clone(),
                    });
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
        let StringInfo { name, .. } = state.name.clone();
        if let StateType::State(component, _initial, transitions) = &state.typ {
            match component {
                None => {
                    transitions
                        .iter()
                        .for_each(|t| self.add_transition(prefix, &name, comps_input, t));
                }
                Some(comp) => self.add_blueprint_state(
                    prefix,
                    &state.name,
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
        automaton: &StringInfo,
        automata: &HashMap<String, &Automaton>,
        visited: &HashSet<String>,
    ) -> Option<()> {
        let StringInfo { name, info } = automaton;
        if !automata.contains_key(name) {
            self.errors.push(Error::Unknown {
                typ: "automaton".to_string(),
                found: name.clone(),
                info: info.clone(),
            });
            return None;
        }
        if visited.contains(name) {
            self.errors.push(Error::Cycle {
                typ: "component".to_string(),
                name: name.clone(),
                info: info.clone(),
            });
            return None;
        }
        if self.initial_states.contains_key(name) {
            self.errors.push(Error::Defined {
                typ: "Automaton".to_string(),
                name: name.clone(),
                info: info.clone(),
            })
        }
        Some(())
    }

    // Returns initial state and final states
    fn add_automaton(
        &mut self,
        automaton: &StringInfo,
        automata: &HashMap<String, &Automaton>,
        visited: &mut HashSet<String>,
        prefix: &String,
    ) -> Option<(String, Vec<(String, bool)>)> {
        self.validate_automaton(automaton, automata, visited)?;
        let StringInfo { name, info } = automaton;
        visited.insert(name.clone());
        // Recursively add components
        let mut comps_input: HashMap<String, String> = HashMap::new();
        let mut comps_output: HashMap<String, Vec<(String, bool)>> = HashMap::new();
        let mut aliases: HashSet<String> = HashSet::new();
        for (
            auto,
            StringInfo {
                name: comp,
                info: cinfo,
            },
        ) in automata[name].components.iter()
        {
            if aliases.contains(comp) {
                self.errors.push(Error::Defined {
                    typ: "Alias".to_string(),
                    name: comp.clone(),
                    info: cinfo.clone(),
                });
                continue;
            }
            aliases.insert(comp.clone());
            if let Some((comp_input, mut comp_outputs)) =
                self.add_automaton(&auto, automata, visited, &format!("{}{}.", prefix, comp))
            {
                comps_input.insert(comp.clone(), comp_input);
                comps_output
                    .entry(comp.clone())
                    .or_insert(Vec::new())
                    .append(&mut comp_outputs);
            }
        }
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
                StateType::Accept => Some((state.name.name.clone(), true)),
                StateType::Reject => Some((state.name.name.clone(), false)),
                _ => None,
            })
            .collect();
        // Check initial state validity
        let mut initial_state: Option<String> = None;
        if automata[name].states.is_empty() {
            self.errors.push(Error::NotAllowed {
                reason: "Automaton without states".to_string(),
                info: info.clone(),
            });
        }
        for state in automata[name].states.iter() {
            if let StateType::State(comp, true, _) = &state.typ {
                if comp.is_none() {
                    if initial_state.is_some() {
                        self.errors.push(Error::NotAllowed {
                            reason: "Having multiple initial states".to_string(),
                            info: state.name.info.clone(),
                        });
                    } else {
                        initial_state = Some(state.name.name.clone());
                    }
                } else {
                    self.errors.push(Error::NotAllowed {
                        reason: "Marking component state as initial".to_string(),
                        info: state.name.info.clone(),
                    });
                }
            }
        }

        (initial_state?, final_states).into()
    }
}

// Add automatic sink state for full coverage
fn add_sink_states(automata: &mut Vec<Automaton>) {
    for automaton in automata.iter_mut() {
        let mut sink = false;
        for state in automaton.states.iter_mut() {
            match &mut state.typ {
                StateType::State(_, _, transitions) => {
                    if !transitions.iter().any(|t| t.read == '_') {
                        transitions.push(Transition {
                            read: '_',
                            write: '_',
                            mov: Move::N,
                            state: (
                                StringInfo {
                                    name: "sink".to_string(),
                                    info: automaton.name.info.clone(),
                                },
                                None,
                            ),
                        });
                        sink = true;
                    }
                }
                _ => {}
            }
        }
        if sink {
            automaton.states.push(State {
                name: StringInfo {
                    name: "sink".to_string(),
                    info: automaton.name.info.clone(),
                },
                typ: StateType::Reject,
                desc: "generated sink state".to_string(),
            });
        }
    }
}

pub fn flatten_automata(mut program: Vec<Automaton>) -> FAIR {
    add_sink_states(&mut program);
    let mut visited = HashSet::new();
    let automata: HashMap<_, _> = program
        .iter()
        .map(|auto| (auto.name.name.clone(), auto))
        .collect();
    let mut ir = FAIR::new();
    for automaton in program.iter() {
        if let Some((initial, _)) = ir.add_automaton(
            &automaton.name,
            &automata,
            &mut visited,
            &format!("{}.", automaton.name.name),
        ) {
            ir.initial_states.insert(
                automaton.name.name.clone(),
                format!("{}.{}", automaton.name.name, initial),
            );
        }
    }
    ir
}

pub fn flatten_automaton(mut program: Vec<Automaton>, automaton: String) -> FAIR {
    add_sink_states(&mut program);
    let mut visited = HashSet::new();
    let automata: HashMap<_, _> = program
        .iter()
        .map(|auto| (auto.name.name.clone(), auto))
        .collect();
    let mut ir = FAIR::new();
    if automata.contains_key(&automaton) {
        if let Some((initial, _)) = ir.add_automaton(
            &automata[&automaton].name,
            &automata,
            &mut visited,
            &"".to_string(),
        ) {
            ir.initial_states
                .insert(automata[&automaton].name.name.clone(), initial);
        }
    } else {
        ir.errors.push(Error::Other {
            msg: format!("Unknown starting automaton {}", automaton),
        });
    }
    ir
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_flat_automaton() {
        let program = vec![Automaton {
            name: StringInfo::from("main"),
            components: Vec::new(),
            states: vec![
                State {
                    name: StringInfo::from("first"),
                    typ: StateType::State(
                        None,
                        true,
                        vec![
                            Transition {
                                read: '0',
                                write: 'A',
                                mov: Move::L,
                                state: (StringInfo::from("good"), None),
                            },
                            Transition {
                                read: '_',
                                write: 'B',
                                mov: Move::R,
                                state: (StringInfo::from("bad"), None),
                            },
                        ],
                    ),
                    desc: String::new(),
                },
                State {
                    name: StringInfo::from("good"),
                    typ: StateType::Accept,
                    desc: "accepting state".to_string(),
                },
                State {
                    name: StringInfo::from("bad"),
                    typ: StateType::Reject,
                    desc: "rejecting state".to_string(),
                },
            ],
            desc: String::new(),
        }];
        let result = flatten_automata(program);
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
                name: StringInfo::from("move"),
                components: Vec::new(),
                states: vec![
                    State {
                        name: StringInfo::from("q0"),
                        typ: StateType::State(
                            None,
                            true,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::R,
                                state: (StringInfo::from("q1"), None),
                            }],
                        ),
                        desc: "simple state".to_string(),
                    },
                    State {
                        name: StringInfo::from("q1"),
                        typ: StateType::Accept,
                        desc: "final state".to_string(),
                    },
                ],
                desc: "move\none\ncell".to_string(),
            },
            Automaton {
                name: StringInfo::from("add"),
                components: vec![(StringInfo::from("move"), StringInfo::from("m"))],
                states: vec![
                    State {
                        name: StringInfo::from("first"),
                        typ: StateType::State(
                            None,
                            true,
                            vec![
                                Transition {
                                    read: '1',
                                    write: '0',
                                    mov: Move::N,
                                    state: (StringInfo::from("q0"), Some(StringInfo::from("m"))),
                                },
                                Transition {
                                    read: '0',
                                    write: '1',
                                    mov: Move::N,
                                    state: (StringInfo::from("input"), Some(StringInfo::from("m"))),
                                },
                            ],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("q1"),
                        typ: StateType::State(
                            Some(StringInfo::from("m")),
                            false,
                            vec![
                                Transition {
                                    read: 'A',
                                    write: '_',
                                    mov: Move::N,
                                    state: (StringInfo::from("done"), None),
                                },
                                Transition {
                                    read: '_',
                                    write: 'B',
                                    mov: Move::N,
                                    state: (StringInfo::from("ups"), None),
                                },
                            ],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("done"),
                        typ: StateType::Accept,
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("ups"),
                        typ: StateType::Reject,
                        desc: "upsie".to_string(),
                    },
                ],
                desc: "some complicated machine".to_string(),
            },
            Automaton {
                name: StringInfo::from("main"),
                components: vec![
                    (StringInfo::from("add"), StringInfo::from("a1")),
                    (StringInfo::from("add"), StringInfo::from("a2")),
                ],
                states: vec![
                    State {
                        name: StringInfo::from("first"),
                        typ: StateType::State(
                            None,
                            true,
                            vec![
                                Transition {
                                    read: '&',
                                    write: '@',
                                    mov: Move::L,
                                    state: (
                                        StringInfo::from("input"),
                                        Some(StringInfo::from("a1")),
                                    ),
                                },
                                Transition {
                                    read: '_',
                                    write: '2',
                                    mov: Move::N,
                                    state: (
                                        StringInfo::from("first"),
                                        Some(StringInfo::from("a2")),
                                    ),
                                },
                            ],
                        ),
                        desc: "this state is pretty cool huh".to_string(),
                    },
                    State {
                        name: StringInfo::from("output"),
                        typ: StateType::State(
                            Some(StringInfo::from("a1")),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: (StringInfo::from("finally"), None),
                            }],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("accept"),
                        typ: StateType::State(
                            Some(StringInfo::from("a2")),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: (StringInfo::from("finally"), None),
                            }],
                        ),
                        desc: "all accepting state of component a2".to_string(),
                    },
                    State {
                        name: StringInfo::from("reject"),
                        typ: StateType::State(
                            Some(StringInfo::from("a2")),
                            false,
                            vec![Transition {
                                read: '_',
                                write: '_',
                                mov: Move::N,
                                state: (StringInfo::from("double_ups"), None),
                            }],
                        ),
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("finally"),
                        typ: StateType::Accept,
                        desc: String::new(),
                    },
                    State {
                        name: StringInfo::from("double_ups"),
                        typ: StateType::Reject,
                        desc: "you really messed up".to_string(),
                    },
                ],
                desc: "turing machines are cool".to_string(),
            },
        ];
        let ir = flatten_automata(program);
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
