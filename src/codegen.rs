use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::Block;
use cranelift_codegen::ir::SigRef;
use cranelift_codegen::ir::{
    types, AbiParam, Function, InstBuilder, MemFlags, Signature, UserFuncName,
};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::write::{FuncWriter, PlainWriter};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use std::collections::HashMap;
use std::str::FromStr;

use crate::fair::FAIR;

struct LabelledWriter {
    label_blocks: HashMap<String, Block>,
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
    tape_addr: u64,
    write_char_addr: u64,
    exit_addr: u64,
    string_addrs: HashMap<String, (u64, usize)>,
    // for generating program bytes
    builder: FunctionBuilder<'a>,
    label_blocks: HashMap<String, Block>,
    // trampolines
    write_char_sig: SigRef,
    exit_sig: SigRef,
    // tape pointers
    left: Variable,
    right: Variable,
    start: Variable,
    end: Variable,
    index: Variable,
}

impl<'a> CodeGen<'a> {
    // Add instructions for printing a hardcoded string, the given string must first be added to the string table
    // TODO: Optimization for single char?
    fn print_string(&mut self, string: &str) {
        let (str_addr, len) = self.string_addrs[string];
        let target = self
            .builder
            .ins()
            .iconst(types::I64, self.write_char_addr as i64);

        for k in 0..len as u64 {
            let byte_addr = self.builder.ins().iconst(types::I64, (str_addr + k) as i64);
            self.builder
                .ins()
                .call_indirect(self.write_char_sig, target, &[byte_addr]);
        }
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
        let one = self.builder.ins().iconst(types::I64, 1);
        let tape_base = self.builder.ins().iconst(types::I64, self.tape_addr as i64);
        let write_target = self
            .builder
            .ins()
            .iconst(types::I64, self.write_char_addr as i64);

        self.print_string("..");
        self.print_string("@");

        let start_val = self.builder.use_var(self.start);
        let first_val = self.builder.ins().iadd(start_val, one);
        self.builder.def_var(self.index, first_val);
        let left_val = self.builder.use_var(self.left);
        let limit_val = self.builder.ins().iadd(left_val, one);
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
            .call_indirect(self.write_char_sig, write_target, &[addr]);
        let new_index = self.builder.ins().iadd(index_val, one);
        self.builder.def_var(self.index, new_index);
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
            .call_indirect(self.write_char_sig, write_target, &[addr]);
        self.print_string("|");
        let new_index = self.builder.ins().iadd(index_val, one);
        self.builder.def_var(self.index, new_index);
        self.builder.ins().jump(right_check_block, &[]);

        self.builder.switch_to_block(right_done_block);
        self.print_string("@");
        self.print_string("..");
        self.print_string("\n");
    }

    fn store_input(&mut self) {
        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        let argv_ptr: cranelift_codegen::ir::Value = self.builder.block_params(entry_block)[0];

        // Invariants
        let zero = self.builder.ins().iconst(types::I64, 0);
        let one = self.builder.ins().iconst(types::I64, 1);
        let blank = self.builder.ins().iconst(types::I8, '@' as i64);
        let tape_base = self.builder.ins().iconst(types::I64, self.tape_addr as i64);

        // Init tape pointers
        self.builder.declare_var(self.left, types::I64);
        self.builder.declare_var(self.right, types::I64);
        self.builder.declare_var(self.start, types::I64);
        self.builder.declare_var(self.end, types::I64);
        self.builder.declare_var(self.index, types::I64);
        self.builder.def_var(self.right, zero);
        self.builder.def_var(self.start, zero);

        // Start with a @ for the case of no input
        self.builder
            .ins()
            .store(MemFlags::new(), blank, tape_base, 0);

        // Copy the optional argv string into the tape
        let cursor = Variable::from_u32(5);
        self.builder.declare_var(cursor, types::I64);
        self.builder.def_var(cursor, argv_ptr);

        let check_block = self.builder.create_block();
        let copy_block = self.builder.create_block();
        let store_block = self.builder.create_block();
        let done_block = self.builder.create_block();

        self.builder.ins().jump(check_block, &[]);
        self.builder.switch_to_block(check_block);
        let ptr_now = self.builder.use_var(cursor);
        let is_null = self.builder.ins().icmp_imm(IntCC::Equal, ptr_now, 0);
        self.builder
            .ins()
            .brif(is_null, done_block, &[], copy_block, &[]);

        self.builder.switch_to_block(copy_block);
        let ptr_now = self.builder.use_var(cursor);
        let byte = self
            .builder
            .ins()
            .load(types::I8, MemFlags::new(), ptr_now, 0);
        let at_end = self.builder.ins().icmp_imm(IntCC::Equal, byte, 0);
        self.builder
            .ins()
            .brif(at_end, done_block, &[], store_block, &[]);

        self.builder.switch_to_block(store_block);
        let start_val = self.builder.use_var(self.start);
        let tape_slot = self.builder.ins().iadd(tape_base, start_val);
        self.builder
            .ins()
            .store(MemFlags::new(), byte, tape_slot, 0);
        // Move start to the right
        let new_start = self.builder.ins().iadd(start_val, one);
        self.builder.def_var(self.start, new_start);
        // Move cursor to the right
        let new_ptr = self.builder.ins().iadd(ptr_now, one);
        self.builder.def_var(cursor, new_ptr);
        self.builder.ins().jump(check_block, &[]);

        self.builder.switch_to_block(done_block);
        // Set start = max(0, start - 1)
        let start_val = self.builder.use_var(self.start);
        let minus_one = self.builder.ins().isub(start_val, one);
        let new_start = self.builder.ins().smax(minus_one, zero);
        self.builder.def_var(self.start, new_start);
        // Set left = start & end = start + 1
        self.builder.def_var(self.left, new_start);
        let new_end = self.builder.ins().iadd(new_start, one);
        self.builder.def_var(self.end, new_end);
    }

    /// Compiles `program` into raw x86-64 machine code bytes
    pub fn generate_labels(mut self) -> HashMap<String, Block> {
        // Store tape input in the vector
        self.store_input();
        self.print_tape();
        // Create a block for each state
        for state in self.ir.transition_states.iter() {
            self.label_blocks
                .insert(state.clone(), self.builder.create_block());
        }

        // Print each state
        let states = self
            .ir
            .transition_states
            .clone()
            .into_iter()
            .collect::<Vec<_>>();

        let target = self.label_blocks[&states[0]];
        self.builder.ins().jump(target, &[]);
        for i in 0..states.len() {
            let state = &states[i];
            let target = self.label_blocks[state];
            self.builder.switch_to_block(target);

            self.print_string("state ");
            self.print_string(&states[i]);
            self.print_string("\n");

            if i < states.len() - 1 {
                // jump to next state
                let target = self.label_blocks[&states[i + 1]];
                self.builder.ins().jump(target, &[]);
            }
        }

        // exit program
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
    tape_addr: u64,
    write_char_addr: u64,
    exit_addr: u64,
    string_addrs: HashMap<String, (u64, usize)>,
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
    let write_char_sig = builder.import_signature({
        let mut s = Signature::new(CallConv::SystemV);
        s.params.push(AbiParam::new(types::I64)); // pointer to the byte
        s
    });
    let exit_sig = builder.import_signature(Signature::new(CallConv::SystemV));

    let cg = CodeGen {
        ir,
        tape_addr,
        write_char_addr,
        exit_addr,
        string_addrs,
        builder,
        label_blocks: HashMap::new(),
        write_char_sig,
        exit_sig,
        left: Variable::from_u32(0),
        right: Variable::from_u32(1),
        start: Variable::from_u32(2),
        end: Variable::from_u32(3),
        index: Variable::from_u32(4),
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
