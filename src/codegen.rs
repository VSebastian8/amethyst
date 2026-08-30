use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::Block;
use cranelift_codegen::ir::SigRef;
use cranelift_codegen::ir::{
    types, AbiParam, Function, InstBuilder, MemFlags, Signature, UserFuncName, Value,
};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::write::{FuncWriter, PlainWriter};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use std::collections::HashMap;
use std::rc::Rc;
use std::str::FromStr;

use crate::ast::Move;
use crate::fair::FAIR;

struct LabelledWriter {
    label_blocks: HashMap<Rc<str>, Block>,
}

impl FuncWriter for LabelledWriter {
    fn write_block_header(
        &mut self,
        w: &mut dyn std::fmt::Write,
        func: &cranelift_codegen::ir::Function,
        block: Block,
        indent: usize,
    ) -> std::fmt::Result {
        cranelift_codegen::write::PlainWriter.write_block_header(w, func, block, indent)?;
        if let Some((name, _)) = self.label_blocks.iter().find(|(_, b)| **b == block) {
            writeln!(w, "; -> state {name:?}")?;
        }
        Ok(())
    }

    fn write_instruction(
        &mut self,
        w: &mut dyn std::fmt::Write,
        func: &cranelift_codegen::ir::Function,
        aliases: &cranelift_codegen::entity::SecondaryMap<
            cranelift_codegen::ir::Value,
            Vec<cranelift_codegen::ir::Value>,
        >,
        inst: cranelift_codegen::ir::Inst,
        indent: usize,
    ) -> std::fmt::Result {
        PlainWriter.write_instruction(w, func, aliases, inst, indent)
    }
}

struct CodeGen<'a> {
    ir: FAIR,
    memory: u64,
    tape_addr: i32,
    write_str_addr: u64,
    exit_addr: u64,
    string_addrs: HashMap<Rc<str>, (u64, usize)>,
    // for generating program bytes
    builder: FunctionBuilder<'a>,
    label_blocks: HashMap<Rc<str>, Block>,
    // trampolines
    write_sig: SigRef,
    exit_sig: SigRef,
    // tape pointers
    left: Variable,
    right: Variable,
    start: Variable,
    end: Variable,
    cursor: Variable,
    index: Variable,
    cells: Variable,
}

impl<'a> CodeGen<'a> {
    fn declare_vars(&mut self) {
        self.builder.declare_var(self.left, types::I64);
        self.builder.declare_var(self.right, types::I64);
        self.builder.declare_var(self.start, types::I64);
        self.builder.declare_var(self.end, types::I64);
        self.builder.declare_var(self.cursor, types::I64);
        self.builder.declare_var(self.index, types::I64);
        self.builder.declare_var(self.cells, types::I64);
    }

    // Add instructions for printing a hardcoded string, the given string must first be added to the string table
    fn print_string(&mut self, string: &str) {
        let (str_addr, len) = self.string_addrs[string];
        let ptr = self.builder.ins().iconst(types::I64, str_addr as i64);
        let len_val = self.builder.ins().iconst(types::I64, len as i64);
        let target = self
            .builder
            .ins()
            .iconst(types::I64, self.write_str_addr as i64);
        self.builder
            .ins()
            .call_indirect(self.write_sig, target, &[ptr, len_val]);
    }

    // var = var + 1
    fn plus_var(&mut self, var: Variable) {
        let val = self.builder.use_var(var);
        let plus_one = self.builder.ins().iadd_imm(val, 1);
        self.builder.def_var(var, plus_one);
    }

    // var = (var + 1) % memory
    fn inc_var(&mut self, var: Variable) {
        let val = self.builder.use_var(var);
        let at_max = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, val, self.memory as i64 - 1);
        let plus_one = self.builder.ins().iadd_imm(val, 1);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let wrapped = self.builder.ins().select(at_max, zero, plus_one);
        self.builder.def_var(var, wrapped);
    }

    // var = (var - 1) % memory
    fn dec_var(&mut self, var: Variable) {
        let val = self.builder.use_var(var);
        let at_zero = self.builder.ins().icmp_imm(IntCC::Equal, val, 0);
        let minus_one = self.builder.ins().iadd_imm(val, -1);
        let tape_end = self
            .builder
            .ins()
            .iconst(types::I64, self.memory as i64 - 1);
        let wrapped = self.builder.ins().select(at_zero, tape_end, minus_one);
        self.builder.def_var(var, wrapped);
    }

    fn entry(&mut self) {
        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        let argv_ptr: cranelift_codegen::ir::Value = self.builder.block_params(entry_block)[0];
        let zero = self.builder.ins().iconst(types::I64, 0);
        // Init tape pointers
        self.builder.def_var(self.right, zero);
        self.builder.def_var(self.start, zero);
        self.builder.def_var(self.cells, zero);
        self.builder.def_var(self.cursor, argv_ptr);
    }

    fn store_input(&mut self) {
        // Invariants
        let zero = self.builder.ins().iconst(types::I64, 0);
        let one = self.builder.ins().iconst(types::I64, 1);
        let blank = self.builder.ins().iconst(types::I8, '@' as i64);
        // Start with a @ for the case of no input
        self.builder
            .ins()
            .store(MemFlags::new(), blank, zero, self.tape_addr);
        // Copy the optional argv string into the tape
        let check_block = self.builder.create_block();
        let copy_block = self.builder.create_block();
        let validate_block = self.builder.create_block();
        let invalid_block = self.builder.create_block();
        let store_block = self.builder.create_block();
        let done_block = self.builder.create_block();

        self.builder.ins().jump(check_block, &[]);
        self.builder.switch_to_block(check_block);
        let ptr_now = self.builder.use_var(self.cursor);
        let is_null = self.builder.ins().icmp_imm(IntCC::Equal, ptr_now, 0);
        self.builder
            .ins()
            .brif(is_null, done_block, &[], copy_block, &[]);

        self.builder.switch_to_block(copy_block);
        let ptr_now = self.builder.use_var(self.cursor);
        let byte = self
            .builder
            .ins()
            .load(types::I8, MemFlags::new(), ptr_now, 0);
        let at_end = self.builder.ins().icmp_imm(IntCC::Equal, byte, 0);
        self.builder
            .ins()
            .brif(at_end, done_block, &[], validate_block, &[]);

        // Input character must be one of  A-Z | 0-9 | @ | &
        self.builder.switch_to_block(validate_block);
        let is_amp = self.builder.ins().icmp_imm(IntCC::Equal, byte, '&' as i64);
        let is_digit_lo =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedGreaterThanOrEqual, byte, '0' as i64);
        let is_digit_hi =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedLessThanOrEqual, byte, '9' as i64);
        let is_digit = self.builder.ins().band(is_digit_lo, is_digit_hi);
        let is_at = self.builder.ins().icmp_imm(IntCC::Equal, byte, '@' as i64);
        let is_alpha_lo =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedGreaterThanOrEqual, byte, 'A' as i64);
        let is_alpha_hi =
            self.builder
                .ins()
                .icmp_imm(IntCC::SignedLessThanOrEqual, byte, 'Z' as i64);
        let is_alpha = self.builder.ins().band(is_alpha_lo, is_alpha_hi);
        let valid = self.builder.ins().bor(is_amp, is_digit);
        let valid = self.builder.ins().bor(valid, is_at);
        let valid = self.builder.ins().bor(valid, is_alpha);
        self.builder
            .ins()
            .brif(valid, store_block, &[], invalid_block, &[]);

        self.builder.switch_to_block(invalid_block);
        let write_target = self
            .builder
            .ins()
            .iconst(types::I64, self.write_str_addr as i64);
        self.print_string("Invalid character `");
        self.builder
            .ins()
            .call_indirect(self.write_sig, write_target, &[ptr_now, one]);
        self.print_string("` in input!");
        self.print_string("\n");
        let target = self.builder.ins().iconst(types::I64, self.exit_addr as i64);
        self.builder.ins().call_indirect(self.exit_sig, target, &[]);
        self.builder.ins().return_(&[]);

        self.builder.switch_to_block(store_block);
        let start_val = self.builder.use_var(self.start);
        self.builder
            .ins()
            .store(MemFlags::new(), byte, start_val, self.tape_addr);
        // Move start and cursor to the right
        self.plus_var(self.start);
        self.plus_var(self.cursor);
        self.builder.ins().jump(check_block, &[]);

        self.builder.switch_to_block(done_block);
        // Set start = max(0, start - 1)
        let start_val = self.builder.use_var(self.start);
        self.builder.def_var(self.cells, start_val);
        let minus_one = self.builder.ins().isub(start_val, one);
        let new_start = self.builder.ins().smax(minus_one, zero);
        self.builder.def_var(self.start, new_start);
        // Set left = start & end = start + 1
        self.builder.def_var(self.left, new_start);
        let new_end = self.builder.ins().iadd(new_start, one);
        self.builder.def_var(self.end, new_end);
    }

    // Add instructions for printing the tape content
    fn print_tape(&mut self) {
        // Blocks
        let left_check_block = self.builder.create_block();
        let left_print_block = self.builder.create_block();
        let left_done_block = self.builder.create_block();
        let right_check_block = self.builder.create_block();
        let right_print_block = self.builder.create_block();
        let right_done_block = self.builder.create_block();
        // Invariants
        let tape_base = self.builder.ins().iconst(types::I64, self.tape_addr as i64);
        let write_target = self
            .builder
            .ins()
            .iconst(types::I64, self.write_str_addr as i64);

        self.print_string("..");
        self.print_string("@");

        let start_val = self.builder.use_var(self.start);
        self.builder.def_var(self.index, start_val);
        self.inc_var(self.index);
        let left_val = self.builder.use_var(self.left);
        let at_max = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, left_val, self.memory as i64 - 1);
        let plus_one = self.builder.ins().iadd_imm(left_val, 1);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let one = self.builder.ins().iconst(types::I64, 1);
        let limit_val = self.builder.ins().select(at_max, zero, plus_one);
        self.builder.ins().jump(left_check_block, &[]);

        // Left stack
        self.builder.switch_to_block(left_check_block);
        let index_val = self.builder.use_var(self.index);
        let is_finished = self.builder.ins().icmp(IntCC::Equal, index_val, limit_val);
        self.builder
            .ins()
            .brif(is_finished, left_done_block, &[], left_print_block, &[]);

        self.builder.switch_to_block(left_print_block);
        self.print_string("|");
        let addr = self.builder.ins().iadd(tape_base, index_val);
        self.builder
            .ins()
            .call_indirect(self.write_sig, write_target, &[addr, one]);
        self.inc_var(self.index);
        self.builder.ins().jump(left_check_block, &[]);

        self.builder.switch_to_block(left_done_block);
        self.print_string("|");
        let right_val = self.builder.use_var(self.right);
        self.builder.def_var(self.index, right_val);
        let limit_val = self.builder.use_var(self.end);
        self.builder.ins().jump(right_check_block, &[]);
        // Right stack
        self.builder.switch_to_block(right_check_block);
        let index_val = self.builder.use_var(self.index);
        let is_finished = self.builder.ins().icmp(IntCC::Equal, index_val, limit_val);
        self.builder
            .ins()
            .brif(is_finished, right_done_block, &[], right_print_block, &[]);

        self.builder.switch_to_block(right_print_block);
        let addr = self.builder.ins().iadd(tape_base, index_val);
        self.builder
            .ins()
            .call_indirect(self.write_sig, write_target, &[addr, one]);
        self.print_string("|");
        self.inc_var(self.index);
        self.builder.ins().jump(right_check_block, &[]);

        self.builder.switch_to_block(right_done_block);
        self.print_string("@");
        self.print_string("..");
        self.print_string("\n");
    }

    // cells >= memory => overflow
    fn check_memory(&mut self) {
        let exit_block = self.builder.create_block();
        let done_block = self.builder.create_block();
        // Check stacks
        let cells_val = self.builder.use_var(self.cells);
        let overflow = self.builder.ins().icmp_imm(
            IntCC::UnsignedGreaterThanOrEqual,
            cells_val,
            self.memory as i64,
        );
        self.builder
            .ins()
            .brif(overflow, exit_block, &[], done_block, &[]);
        self.builder.switch_to_block(exit_block);
        self.print_string(&format!("Memory limit {} exceeded!\n", self.memory));
        let target = self.builder.ins().iconst(types::I64, self.exit_addr as i64);
        self.builder.ins().call_indirect(self.exit_sig, target, &[]);
        self.builder.ins().return_(&[]);
        self.builder.switch_to_block(done_block);
    }

    fn move_left(&mut self) {
        let blank_block = self.builder.create_block();
        let sym_block = self.builder.create_block();
        let done_block = self.builder.create_block();
        // right--
        self.dec_var(self.right);
        let right_val = self.builder.use_var(self.right);
        // check whether left == start
        let left_val = self.builder.use_var(self.left);
        let start_val = self.builder.use_var(self.start);
        let left_blank = self.builder.ins().icmp(IntCC::Equal, left_val, start_val);
        self.builder
            .ins()
            .brif(left_blank, blank_block, &[], sym_block, &[]);
        // Produce a @ and don't move left
        self.builder.switch_to_block(blank_block);
        let blank = self.builder.ins().iconst(types::I8, '@' as i64);
        self.builder
            .ins()
            .store(MemFlags::new(), blank, right_val, self.tape_addr);
        // new cell has been used
        self.plus_var(self.cells);
        self.check_memory();
        self.builder.ins().jump(done_block, &[]);
        // Move actual symbol and left--
        self.builder.switch_to_block(sym_block);
        let symbol = self
            .builder
            .ins()
            .load(types::I8, MemFlags::new(), left_val, self.tape_addr);
        self.builder
            .ins()
            .store(MemFlags::new(), symbol, right_val, self.tape_addr);
        self.dec_var(self.left);
        self.builder.ins().jump(done_block, &[]);
        self.builder.switch_to_block(done_block);
    }

    fn move_right(&mut self) {
        let blank_block = self.builder.create_block();
        let sym_block = self.builder.create_block();
        let done_block = self.builder.create_block();
        // left++
        self.inc_var(self.left);
        let left_val = self.builder.use_var(self.left);
        // check whether right == end
        let right_val = self.builder.use_var(self.right);
        let end_val = self.builder.use_var(self.end);
        let right_blank = self.builder.ins().icmp(IntCC::Equal, right_val, end_val);
        self.builder
            .ins()
            .brif(right_blank, blank_block, &[], sym_block, &[]);
        // Produce a @ and don't move right
        self.builder.switch_to_block(blank_block);
        let blank = self.builder.ins().iconst(types::I8, '@' as i64);
        self.builder
            .ins()
            .store(MemFlags::new(), blank, left_val, self.tape_addr);
        // new cell has been used
        self.plus_var(self.cells);
        self.check_memory();
        self.builder.ins().jump(done_block, &[]);
        // Move actual symbol and right++
        self.builder.switch_to_block(sym_block);
        let symbol = self
            .builder
            .ins()
            .load(types::I8, MemFlags::new(), right_val, self.tape_addr);
        self.builder
            .ins()
            .store(MemFlags::new(), symbol, left_val, self.tape_addr);
        self.inc_var(self.right);
        self.builder.ins().jump(done_block, &[]);
        self.builder.switch_to_block(done_block);
    }

    fn write(&mut self, symbol: char) {
        let empty_block = self.builder.create_block();
        let write_block = self.builder.create_block();
        // If right stack is empty, push a new value, otherwise rewrite top value
        let right_val = self.builder.use_var(self.right);
        let end_val = self.builder.use_var(self.end);
        let right_empty = self.builder.ins().icmp(IntCC::Equal, right_val, end_val);
        self.builder
            .ins()
            .brif(right_empty, empty_block, &[], write_block, &[]);

        self.builder.switch_to_block(empty_block);
        self.dec_var(self.right);
        // new cell has been used
        self.plus_var(self.cells);
        self.check_memory();
        self.builder.ins().jump(write_block, &[]);

        self.builder.switch_to_block(write_block);
        let symbol_val = self.builder.ins().iconst(types::I8, symbol as i64);
        let right_val = self.builder.use_var(self.right);
        self.builder
            .ins()
            .store(MemFlags::new(), symbol_val, right_val, self.tape_addr);
    }

    fn read(&mut self) -> Value {
        let read_block = self.builder.create_block();
        let blank_block = self.builder.create_block();
        let done_block = self.builder.create_block();
        self.builder.append_block_param(done_block, types::I8);
        // If right stack is empty, read @, otherwise read top value
        let right_val = self.builder.use_var(self.right);
        let end_val = self.builder.use_var(self.end);
        let right_empty = self.builder.ins().icmp(IntCC::Equal, right_val, end_val);
        self.builder
            .ins()
            .brif(right_empty, blank_block, &[], read_block, &[]);
        // @
        self.builder.switch_to_block(blank_block);
        let blank = self.builder.ins().iconst(types::I8, '@' as i64);
        self.builder.ins().jump(done_block, &[blank]);
        // top right stack symbol
        self.builder.switch_to_block(read_block);
        let right_val = self.builder.use_var(self.right);
        let symbol = self
            .builder
            .ins()
            .load(types::I8, MemFlags::new(), right_val, self.tape_addr);
        self.builder.ins().jump(done_block, &[symbol]);
        // Return the value as a block parameter
        self.builder.switch_to_block(done_block);
        self.builder.block_params(done_block)[0]
    }

    fn transition_case(
        &mut self,
        read_symbol: char,
        write_symbol: char,
        move_symbol: Move,
        new_state: Rc<str>,
    ) {
        let match_block = self.builder.create_block();
        let next_block = self.builder.create_block();
        // Check if transition applies
        let read_val = self.builder.ins().iconst(types::I8, read_symbol as i64);
        let tape_val = self.read();
        let symbol_match = self.builder.ins().icmp(IntCC::Equal, read_val, tape_val);
        self.builder
            .ins()
            .brif(symbol_match, match_block, &[], next_block, &[]);
        // Apply transition
        self.builder.switch_to_block(match_block);
        if write_symbol != '_' {
            self.write(write_symbol);
        }
        match move_symbol {
            Move::L => {
                self.move_left();
            }
            Move::R => {
                self.move_right();
            }
            Move::N => {}
        }
        self.builder.ins().jump(self.label_blocks[&new_state], &[]);
        // Skip transition
        self.builder.switch_to_block(next_block);
    }

    fn transition_default(&mut self, write_symbol: char, move_symbol: Move, new_state: Rc<str>) {
        if write_symbol != '_' {
            self.write(write_symbol);
        }
        match move_symbol {
            Move::L => {
                self.move_left();
            }
            Move::R => {
                self.move_right();
            }
            Move::N => {}
        }
        self.builder.ins().jump(self.label_blocks[&new_state], &[]);
    }

    /// Compiles `program` into raw x86-64 machine code bytes
    pub fn generate_labels(mut self) -> HashMap<Rc<str>, Block> {
        let init_block = self.builder.create_block();
        let exit_block = self.builder.create_block();
        self.declare_vars();
        // Store tape input in the vector
        self.entry();
        self.store_input();
        self.builder.ins().jump(init_block, &[]);
        // Create a block for each state
        for state in self.ir.accept_states.clone() {
            let block = self.builder.create_block();
            self.label_blocks.insert(state.clone(), block);
            self.builder.switch_to_block(block);
            self.print_string("Accepted by final state ");
            self.print_string(&state);
            self.print_string("\n");
            self.builder.ins().jump(exit_block, &[]);
        }
        for state in self.ir.reject_states.clone() {
            let block = self.builder.create_block();
            self.label_blocks.insert(state.clone(), block);
            self.builder.switch_to_block(block);
            self.print_string("Rejected by final state ");
            self.print_string(&state);
            self.print_string("\n");
            self.builder.ins().jump(exit_block, &[]);
        }
        // First create state blocks without transitions, in order to reference them later
        for state in self.ir.transition_states.iter() {
            let block = self.builder.create_block();
            self.label_blocks.insert(state.clone(), block);
        }
        for (state, transitions) in self.ir.transitions.clone() {
            self.builder.switch_to_block(self.label_blocks[&state]);
            self.print_string("State ");
            self.print_string(&state);
            self.print_string("\n");
            // Default case
            let (write_symbol, move_symbol, new_state) = transitions[&'_'].clone();
            // Other cases
            for (read_symbol, (write_symbol, move_symbol, new_state)) in transitions {
                if read_symbol != '_' {
                    self.transition_case(read_symbol, write_symbol, move_symbol, new_state);
                }
            }
            // If nothing else matched, apply catch-all transition
            self.transition_default(write_symbol, move_symbol, new_state);
        }

        self.builder.switch_to_block(init_block);
        self.print_tape();
        let initial_state = self.ir.initial_states.values().next().unwrap();
        self.builder
            .ins()
            .jump(self.label_blocks[initial_state], &[]);

        // exit program
        self.builder.switch_to_block(exit_block);
        self.print_tape();
        let target = self.builder.ins().iconst(types::I64, self.exit_addr as i64);
        self.builder.ins().call_indirect(self.exit_sig, target, &[]);
        self.builder.ins().return_(&[]);

        self.builder.seal_all_blocks();
        self.builder.finalize();
        self.label_blocks
    }
}

pub fn compile_program(
    ir: FAIR,
    memory: u64,
    tape_addr: u64,
    write_str_addr: u64,
    exit_addr: u64,
    string_addrs: HashMap<Rc<str>, (u64, usize)>,
    debug: bool,
) -> Vec<u8> {
    let isa = build_x86_64_linux_isa();

    // The whole program compiles to a single function with one parameter that calls the exit syscall rather than returning
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(types::I64));
    let mut func: Function = Function::with_name_signature(UserFuncName::default(), sig);
    let mut fn_ctx: FunctionBuilderContext = FunctionBuilderContext::new();

    let mut builder: FunctionBuilder<'_> = FunctionBuilder::new(&mut func, &mut fn_ctx);

    // Signatures for the two hand-written trampolines
    let write_sig = builder.import_signature({
        let mut s = Signature::new(CallConv::SystemV);
        s.params.push(AbiParam::new(types::I64)); // ptr
        s.params.push(AbiParam::new(types::I64)); // len
        s
    });
    let exit_sig = builder.import_signature(Signature::new(CallConv::SystemV));

    let cg = CodeGen {
        ir,
        memory,
        tape_addr: tape_addr as i32,
        write_str_addr,
        exit_addr,
        string_addrs,
        builder,
        label_blocks: HashMap::new(),
        write_sig,
        exit_sig,
        left: Variable::from_u32(0),
        right: Variable::from_u32(1),
        start: Variable::from_u32(2),
        end: Variable::from_u32(3),
        cursor: Variable::from_u32(4),
        index: Variable::from_u32(5),
        cells: Variable::from_u32(6),
    };
    let label_blocks = cg.generate_labels();
    if debug {
        println!("--- Generated Cranelift IR ---");
        let mut ir_text = String::new();
        cranelift_codegen::write::decorate_function(
            &mut LabelledWriter {
                label_blocks: label_blocks,
            },
            &mut ir_text,
            &func,
        )
        .unwrap();
        println!("{ir_text}");
        println!("-------------------------------\n");
    }

    let mut ctx = Context::for_function(func);
    let compiled = ctx
        .compile(&*isa, &mut Default::default())
        .expect("Cranelift compilation failed");
    compiled.code_buffer().to_vec()
}

/// Targets x86-64 Linux explicitly
fn build_x86_64_linux_isa() -> std::sync::Arc<dyn cranelift_codegen::isa::TargetIsa> {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    // static, fixed-address, non-PIE executable
    flag_builder.set("is_pic", "false").unwrap();

    let triple = target_lexicon::Triple::from_str("x86_64-unknown-linux-gnu")
        .expect("failed to parse target triple");
    let isa_builder = cranelift_codegen::isa::lookup(triple).expect("x86-64 backend not available");
    isa_builder
        .finish(settings::Flags::new(flag_builder))
        .expect("failed to finish ISA")
}
