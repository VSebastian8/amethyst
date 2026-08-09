use crate::codegen::*;
use crate::codegen::{Program, Stmt};
use crate::elf::*;
use std::fs;
use std::os::unix::fs::PermissionsExt;

const TAPE_SIZE: u64 = 256;

pub fn compile(name: String) {
    // 1. Build the AST. A real compiler would get this from a
    //    lexer/parser reading a source file; here it's the moral
    //    equivalent of a source program that reads:
    //
    //        push 'H'; print; pop
    //        push 'i'; print; pop
    //        jump skip
    //        push 'X'; print; pop   ; dead code -- jumped over
    //      skip:
    //        push '!'; print; pop
    //        push '\n'; print; pop
    let program = Program {
        body: vec![
            Stmt::Push(b'H'),
            Stmt::Print,
            Stmt::Pop,
            Stmt::Print,
            Stmt::Push(b'i'),
            Stmt::Print,
            Stmt::Debug(String::from(" world")),
            Stmt::Pop,
            Stmt::Jump("skip"),
            Stmt::Push(b'X'), // unreachable -- jumped over, not compiled
            Stmt::Print,
            Stmt::Pop,
            Stmt::Label("skip"),
            Stmt::Push(b'!'),
            Stmt::Print,
            Stmt::Pop,
            Stmt::Push(b'\n'),
            Stmt::Print,
            Stmt::Pop,
        ],
    };

    // All printed strings
    let mut string_table: Vec<u8> = Vec::new();
    let mut debug_offsets: Vec<Option<(u64, usize)>> = Vec::with_capacity(program.body.len());
    for stmt in &program.body {
        match stmt {
            Stmt::Debug(s) => {
                let offset = string_table.len() as u64;
                string_table.extend_from_slice(s.as_bytes());
                debug_offsets.push(Some((offset, s.as_bytes().len())));
            }
            _ => debug_offsets.push(None),
        }
    }

    // 2. Decide the whole binary's memory layout BEFORE compiling
    //    anything. This is the trick that lets us skip relocations
    //    entirely: every address the compiled program needs (the
    //    trampolines, the tape) is a plain compile-time constant by the
    //    time codegen runs, because we chose it ourselves.
    //

    //    Layout, in order: [ELF headers] [write_char] [exit] [_start] [tape] [compiled program]
    let write_char_addr = BASE_ADDR + HEADER_SIZE;
    let exit_addr = write_char_addr + WRITE_CHAR.len() as u64;
    let start_stub_addr = exit_addr + EXIT_PROCESS.len() as u64;
    let tape_addr = start_stub_addr + 17;
    let string_table_addr = tape_addr + TAPE_SIZE;
    let program_addr = string_table_addr + string_table.len() as u64;

    let debug_addrs: Vec<Option<(u64, usize)>> = debug_offsets
        .into_iter()
        .map(|opt| opt.map(|(offset, len)| (string_table_addr + offset, len)))
        .collect();

    println!("Layout:");
    println!("  write_char trampoline @ {write_char_addr:#x}");
    println!("  exit trampoline       @ {exit_addr:#x}");
    println!("  _start                @ {start_stub_addr:#x}");
    println!("  tape buffer           @ {tape_addr:#x} ({TAPE_SIZE} bytes)");
    println!("  debug strings         @ {debug_addrs:?}");
    println!("  compiled program      @ {program_addr:#x}\n");

    // 3. Compile the program with Cranelift, now that it can bake in
    //    those addresses as plain constants.
    let compiled_program = compile_program(
        &program,
        tape_addr,
        write_char_addr,
        exit_addr,
        &debug_addrs,
    );
    println!(
        "Compiled program to {} bytes of machine code.\n",
        compiled_program.len()
    );

    // 4. Assemble the final segment contents in the same order we
    //    promised above, then hand-write the ELF wrapper around it.
    let mut code = Vec::new();
    code.extend_from_slice(&WRITE_CHAR);
    code.extend_from_slice(&EXIT_PROCESS);
    let entry_offset = code.len() as u64; // _start begins here -- this is the real ELF entry point
    code.extend_from_slice(&start_stub(program_addr));
    code.extend(std::iter::repeat(0u8).take(TAPE_SIZE as usize)); // zeroed tape
    code.extend_from_slice(&string_table); // literal debug-string bytes
    code.extend_from_slice(&compiled_program);

    let elf_bytes = build_executable(&code, entry_offset);

    // 5. Write the file and mark it executable (ELF files don't get +x
    //    by default -- `fs::write` alone would leave it non-runnable).
    let out_path = name.as_str();
    fs::write(out_path, &elf_bytes).expect("failed to write output file");
    let mut perms = fs::metadata(out_path).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(out_path, perms).unwrap();

    println!("Wrote ./{out_path} ({} bytes total).", elf_bytes.len());
    println!("Try it now:  ./{out_path}");
}
