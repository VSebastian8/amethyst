# Amethyst

**Amethyst** is a **programming language** where you write everything using **Turing machines**. Its goal is to help grow the understanding of this popular and powerful **computational model**.

### Requirements:

- ghc (for Haskell)
- gcc (for C)
- cargo (for Rust)

For more information on the syntax, check out [SyntaxExamples](src/SyntaxExamples.hs)

### Macros

**Macros** are a way to **reduce boilerplate code**, whether that be a very common and useful machine or a machine with many repetitive states.

_E.g. to move exactly 8 cells to the right we would need 8 states with each state having the only job of making a right move and going to the next state_.

These are **not functions with variables**, they are just **syntactic sugar** that gets expanded into a specific turing machine at compile time. Each macro-machine will have one initial state (input) and one or more final states (output).

The following macros are present so far:

- `complement` (automata)
- `intersect` (automata, automata)
- `reunion` (automata, automata, automata)
  - the 3 macros above have string parameters representing the name of an existing automata, intersect and reunion take 2 or more automata and their function is intuitive
- `chain` (automata, automata, automata)
  - takes two or more automata and chains them like so:
  - the initial state of the first machine is the initial state of the resulting machine
  - all accept states of the first machine go to the initial state of the second machine and so on
  - all reject states go to a reject state 'sink'
- `repeat` (7, automata)
  - equivalent with a chain of 7 automatas of the same type
- `move` (moveSymbol, number)
  - moves the head left or right a number of positions
- `override` (moveSymbol, number, tapeSymbol)
  - a move that also replaces the tape symbols it encounters
- `place` (string)
  - override to the right with a string of tape symbols, useful for writing simple output
- `shift` (moveSymbol, number)
  - a right shift inserts a number of cells to the right of the current position (all of the cells to the right of the head shift right)
  - a left shift deletes a number of cells to the left of the current position (all of the cells to the right of the head shift left)
