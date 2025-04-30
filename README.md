# Amethyst

**Amethyst** is a **programming language** where you write everything using **Turing machines**. Its goal is to help grow the understanding of this popular and powerful **computational model**.

### Run a .myst file

```bash
cargo run -- code/add.myst -input 110+01 -o -limit 500 -tape
```

Or you can specify the compilation flags in a separate file:

```bash
cargo run -- code/simple.myst -c config/example.txt
```

### Requirements

- ghc (for Haskell)
- gcc (for C)
- cargo (for Rust)

For more information on the syntax, check out [SyntaxExamples](src/SyntaxExamples.hs)

### Components

**Components** are a way to **reuse code**. Syntactically, they look like function arguments for the automata, but they are _static copies_ of the specified turing machines.

_E.g. Let's say we have written a turing machine that adds two numbers together and we would like to reuse this code. We can add this 'add' automaton as a component to the main automaton. We can consider the 'add' machine as a black box, concerning ourselves only with its input and output_.

Each component's initial state is exposed to the parent turing machine. Final states (accepting and rejecting) of the component automata have to be rewriten in order to continue the execution of the parent machine.

### Macros

**Macros** are a way to **reduce boilerplate code**, whether that be a very common and useful machine or a machine with many repetitive states.

_E.g. To move exactly 8 cells to the right we would need 8 states with each state having the only job of making a right move and going to the next state_.

These are **not functions with variables**, they are just **syntactic sugar** that gets expanded into a specific turing machine at compile time. Each macro-machine will have one initial state (input) and at most two final states (accept and reject).

The following macros are present so far:

- `complement` (automaton)
- `intersect` (automaton, automaton)
- `reunion` (automaton, automaton, automaton)
  - the 3 macros above have string parameters representing the name of an existing automaton, intersect and reunion take 2 or more automata and their function is intuitive
- `chain` (automaton, automaton, automaton)
  - takes two or more automata and chains them like so:
  - the initial state of the first machine is the initial state of the resulting machine
  - all accept states of the first machine go to the initial state of the second machine and so on
  - all reject states go to a reject state 'sink'
- `repeat` (7, automata)
  - equivalent with a chain of 7 automata of the same type
- `move` (moveSymbol, number)
  - moves the head left or right a number of positions
- `override` (moveSymbol, number, tapeSymbol)
  - a move that also replaces the tape symbols it encounters
- `place` (string)
  - override to the right with a string of tape symbols, useful for writing simple output
- `shift` (moveSymbol, number)
  - a right shift inserts a number of cells to the right of the current position (all of the cells to the right of the head shift right)
  - a left shift deletes a number of cells to the left of the current position (all of the cells to the right of the head shift left)

### Compilation Flags

|         Flag         | Shorthand  |       Argument       | Default |
| :------------------: | :--------: | :------------------: | :-----: |
|        -input        |     -i     | initial tape content |    @    |
|       -output        |     -o     |                      |  false  |
|        -tape         |     -t     |                      |  false  |
|        -start        |     -s     |    automaton name    |  main   |
|        -bound        |     -b     | max number of cells  |  none   |
| -iterations / -limit | -iter / -l | max number of steps  |  1200   |
|        -debug        |     -d     |                      |  false  |
|       -config        |     -c     |  path/to/config.txt  |         |

### Checking for memory leaks

```bash
cargo run
valgrind target/debug/amethyst --leak-check=full -s
```

### Testing the FFI parser

```bash
cargo test --test parser_tests -- --nocapture --test-threads=1
```

> Runs all tests from tests/parser_tests.rs \
> Shows the output of println! \
> Runs tests sequentially (memory issues if run in parallel)
