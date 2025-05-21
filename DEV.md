### Requirements

- ghc (for Haskell)
- gcc (for C)
- cargo (for Rust)

For more information on the syntax, check out [SyntaxExamples](src/SyntaxExamples.hs)

### Checking for memory leaks

```bash
cargo run
valgrind target/debug/amethyst --leak-check=full -s
```

### Testing

```bash
cargo test --test parser_tests -- --nocapture --test-threads=1
```

> Set parser="haskell" in build.rs
> Runs all tests from tests/parser_tests.rs \
> Shows the output of println! \
> Runs tests sequentially (memory issues if run in parallel)

### Release

```bash
cross build --release --target x86_64-unknown-linux-gnu
cross build --release --target x86_64-pc-windows-gnu
```

### Known Issues

Rust optimization level 2+ breaks the parser combinators (possible because of instruction reordering)
