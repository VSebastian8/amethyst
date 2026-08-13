use crate::ast::Ast;
use crate::compiler::compile_program;
use crate::elf::*;
use crate::fair::{flatten_automaton, FAIR};
use crate::gem::load_ast;
use crate::{codegen::*, info};

use std::collections::HashMap;
use std::fs;
use std::os::unix::fs::PermissionsExt;

const TAPE_SIZE: u64 = 256;

pub fn read_and_compile(
    filename: &String,
    mut automaton: String,
    debug: bool,
) -> Result<(), Vec<info::Error>> {
    let Ast {
        automata,
        mut errors,
    } = match load_ast(filename) {
        Ok(ast) => ast,
        Err(err) => {
            return Err(vec![info::Error::Other {
                msg: err.to_string(),
            }])
        }
    };
    if automaton == "main" && automata.iter().all(|auto| auto.name.name != "main") {
        automaton = automata[0].name.name.clone();
    }
    let mut ir = flatten_automaton(automata, automaton.clone());
    errors.append(&mut ir.errors);
    if !errors.is_empty() {
        return Err(errors);
    } else {
        compile(automaton, ir, debug);
        Ok(())
    }
}

pub fn compile(name: String, ir: FAIR, debug: bool) {
    //    Layout, in order: [ELF headers] [write_char] [exit] [_start] [tape] [strings] [compiled program]
    let write_char_addr = BASE_ADDR + HEADER_SIZE;
    let exit_addr = write_char_addr + WRITE_CHAR.len() as u64;
    let start_stub_addr = exit_addr + EXIT_PROCESS.len() as u64;
    let tape_addr = start_stub_addr + 17;
    let string_table_addr = tape_addr + TAPE_SIZE;

    // All printed strings
    let mut string_table: Vec<u8> = Vec::new();
    let mut string_addrs: HashMap<String, (u64, usize)> = HashMap::new();
    let mut strings: Vec<String> = Vec::from(["\n", "state ", "->", "|", "@", "..", " "])
        .iter()
        .map(|s| s.to_string())
        .collect();
    strings.extend(ir.transition_states.clone().into_iter());

    for string in strings {
        let offset = string_table_addr + string_table.len() as u64;
        string_table.extend_from_slice(string.as_bytes());
        string_addrs.insert(string.clone(), (offset, string.as_bytes().len()));
    }

    let program_addr = string_table_addr + string_table.len() as u64;

    if debug {
        println!("Layout:");
        println!("- trampolines: write @ {write_char_addr:#x} exit @ {exit_addr:#x} start @ {start_stub_addr:#x}");
        println!("- tape buffer        @ {tape_addr:#x} ({TAPE_SIZE} bytes)");
        println!("- string pool        @ {string_addrs:?}");
        println!("- compiled program   @ {program_addr:#x}\n");
    }

    // Compile the program with Cranelift
    let compiled_program = compile_program(
        ir,
        tape_addr,
        write_char_addr,
        exit_addr,
        string_addrs,
        debug,
    );

    // Assemble the final segment contents, then hand-write the ELF wrapper around it.
    let mut code = Vec::new();
    code.extend_from_slice(&WRITE_CHAR);
    code.extend_from_slice(&EXIT_PROCESS);
    let entry_offset = code.len() as u64; // _start begins here -- this is the real ELF entry point
    code.extend_from_slice(&start_stub(program_addr));
    code.extend(std::iter::repeat(0u8).take(TAPE_SIZE as usize)); // zeroed tape
    code.extend_from_slice(&string_table); // literal debug-string bytes
    code.extend_from_slice(&compiled_program);

    let elf_bytes = build_executable(&code, entry_offset);

    // Write the file and mark it executable
    let out_path = name.as_str();
    fs::write(out_path, &elf_bytes).expect("failed to write output file");
    let mut perms = fs::metadata(out_path).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(out_path, perms).unwrap();

    println!("Succesfully compiled ./{out_path}");
}
