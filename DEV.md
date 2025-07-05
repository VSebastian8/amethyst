### Requirements

- cargo

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

### Release

```bash
cross build --release --target x86_64-unknown-linux-gnu
cross build --release --target x86_64-pc-windows-gnu
```

### Known Issues

Rust optimization level 2+ breaks the parser combinators (because of instruction reordering)
