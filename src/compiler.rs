use crate::ast::Ast;
use crate::compiler::compile_program;
use crate::elf::*;
use crate::fair::{flatten_automaton, FAIR};
use crate::gem::load_ast;
use crate::{codegen::*, info};

use std::collections::HashMap;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::rc::Rc;

pub fn read_and_compile(
    filename: &String,
    mut automaton: Rc<str>,
    memory: u64,
    debug: bool,
) -> Result<(), Vec<info::ErrorInfo>> {
    let Ast {
        automata,
        errors: syntax_errors,
    } = match load_ast(filename) {
        Ok(ast) => ast,
        Err(err) => return Err(vec![err]),
    };
    if automaton.as_ref() == "main"
        && automata
            .iter()
            .all(|auto| auto.name.name.as_ref() != "main")
    {
        automaton = automata[0].name.name.clone();
    }
    let ir = flatten_automaton(automata, automaton.clone());
    let errors: Vec<_> = syntax_errors
        .iter()
        .map(|err| info::ErrorInfo {
            error: (*err).clone(),
            info: None,
        })
        .chain(ir.errors.clone().into_iter())
        .collect();
    if !errors.is_empty() {
        return Err(errors);
    } else {
        compile(automaton, ir, memory, debug);
        Ok(())
    }
}

pub fn compile(name: Rc<str>, ir: FAIR, memory: u64, debug: bool) {
    //    Layout, in order: [ELF headers] [write_char] [exit] [_start] [tape] [strings] [compiled program]
    let write_str_addr = BASE_ADDR + HEADER_SIZE;
    let exit_addr = write_str_addr + WRITE_STR.len() as u64;
    let start_stub_addr = exit_addr + EXIT_PROCESS.len() as u64;
    let tape_addr = start_stub_addr + 17;
    let string_table_addr = tape_addr + memory;

    // All printed strings
    let mut string_table: Vec<u8> = Vec::new();
    let mut string_addrs: HashMap<Rc<str>, (u64, usize)> = HashMap::new();
    let mut strings: Vec<Rc<str>> = Vec::from([
        "\n",
        "->",
        "|",
        "@",
        "..",
        " ",
        "State ",
        "Accepted by final state ",
        "Rejected by final state ",
        "Invalid character `",
        "` in input!",
    ])
    .iter()
    .map(|s| (*s).into())
    .collect();
    strings.push(format!("Memory limit {} exceeded!\n", memory).into());
    strings.extend(ir.transition_states.clone().into_iter());
    strings.extend(ir.accept_states.clone().into_iter());
    strings.extend(ir.reject_states.clone().into_iter());

    for string in strings {
        let offset = string_table_addr + string_table.len() as u64;
        string_table.extend_from_slice(string.as_bytes());
        string_addrs.insert(string.clone(), (offset, string.as_bytes().len()));
    }

    let program_addr = string_table_addr + string_table.len() as u64;

    if debug {
        println!("Layout:");
        println!("- trampolines: write @ {write_str_addr:#x} exit @ {exit_addr:#x} start @ {start_stub_addr:#x}");
        println!("- tape buffer        @ {tape_addr:#x} ({memory} bytes)");
        println!("- string pool        @ {string_addrs:?}");
        println!("- compiled program   @ {program_addr:#x}\n");
    }

    // Compile the program with Cranelift
    let compiled_program = compile_program(
        ir,
        memory,
        tape_addr,
        write_str_addr,
        exit_addr,
        string_addrs,
        debug,
    );

    // Assemble the final segment contents, then hand-write the ELF wrapper around it.
    let mut code = Vec::new();
    code.extend_from_slice(&WRITE_STR);
    code.extend_from_slice(&EXIT_PROCESS);
    let entry_offset = code.len() as u64; // _start begins here -- this is the real ELF entry point
    code.extend_from_slice(&start_stub(program_addr));
    code.extend(std::iter::repeat(0u8).take(memory as usize)); // zeroed tape
    code.extend_from_slice(&string_table); // literal debug-string bytes
    code.extend_from_slice(&compiled_program);

    let elf_bytes = build_executable(&code, entry_offset);

    // Write the file and mark it executable
    let out_path = name.as_ref();
    fs::write(out_path, &elf_bytes).expect("failed to write output file");
    let mut perms = fs::metadata(out_path).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(out_path, perms).unwrap();

    println!("Succesfully compiled ./{out_path}");
}
