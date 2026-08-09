use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::Block;
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

/// Mock ast
pub enum Stmt {
    /// Push a byte onto the vector.
    Push(u8),
    /// Remove the top byte from the vector.
    Pop,
    /// Print the current top byte of the vector (without popping it).
    Print,
    /// Print a fixed string, known at compile time -- NOT read from the
    /// vector. Useful for simple debug/trace output.
    Debug(String),
    /// A named jump target. Doesn't do anything by itself -- `Jump`
    /// statements elsewhere reference it by name.
    Label(&'static str),
    /// Unconditionally jump to the block starting at a `Label` with this
    /// name.
    Jump(&'static str),
}

pub struct Program {
    pub body: Vec<Stmt>,
}

struct LabelledWriter<'a> {
    label_blocks: &'a HashMap<&'static str, Block>,
}

impl<'a> FuncWriter for LabelledWriter<'a> {
    fn write_block_header(
        &mut self,
        w: &mut dyn std::fmt::Write,
        func: &cranelift_codegen::ir::Function,
        block: Block,
        indent: usize,
    ) -> std::fmt::Result {
        cranelift_codegen::write::PlainWriter.write_block_header(w, func, block, indent)?;
        if let Some((name, _)) = self.label_blocks.iter().find(|(_, b)| **b == block) {
            writeln!(w, "  ; label {name:?}")?;
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

/// Compiles `program` into raw x86-64 machine code bytes
pub fn compile_program(
    program: &Program,
    tape_addr: u64,
    write_char_addr: u64,
    exit_addr: u64,
    debug_addrs: &[Option<(u64, usize)>],
) -> Vec<u8> {
    let isa = build_x86_64_linux_isa();

    // The whole program compiles to a single function with one parameter that calls the exit syscall rather than returning
    let mut sig = Signature::new(CallConv::SystemV);
    sig.params.push(AbiParam::new(types::I64));
    let mut func = Function::with_name_signature(UserFuncName::default(), sig);
    let mut fn_ctx = FunctionBuilderContext::new();

    // Declared outside for decorations
    let mut label_blocks: HashMap<&str, Block> = HashMap::new();
    {
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_ctx);

        // Signatures for the two hand-written trampolines
        let write_char_sig = builder.import_signature({
            let mut s = Signature::new(CallConv::SystemV);
            s.params.push(AbiParam::new(types::I64)); // pointer to the byte
            s
        });
        let exit_sig = builder.import_signature(Signature::new(CallConv::SystemV));

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        let argv_ptr = builder.block_params(entry_block)[0];

        // tape pointer
        let sp = Variable::from_u32(0);
        builder.declare_var(sp, types::I64);
        let zero = builder.ins().iconst(types::I64, 0);
        builder.def_var(sp, zero);

        // Copy the optional argv string into the tape
        let cursor = Variable::from_u32(1);
        builder.declare_var(cursor, types::I64);
        builder.def_var(cursor, argv_ptr);

        let check_block = builder.create_block();
        let copy_block = builder.create_block();
        let done_block = builder.create_block();
        builder.ins().jump(check_block, &[]);

        builder.switch_to_block(check_block);
        let ptr_now = builder.use_var(cursor);
        let is_null = builder.ins().icmp_imm(IntCC::Equal, ptr_now, 0);
        builder
            .ins()
            .brif(is_null, done_block, &[], copy_block, &[]);

        builder.switch_to_block(copy_block);
        let ptr_now = builder.use_var(cursor);
        let byte = builder.ins().load(types::I8, MemFlags::new(), ptr_now, 0);
        let at_end = builder.ins().icmp_imm(IntCC::Equal, byte, 0);
        let store_block = builder.create_block();
        builder
            .ins()
            .brif(at_end, done_block, &[], store_block, &[]);

        builder.switch_to_block(store_block);
        let sp_val = builder.use_var(sp);
        let tape_base = builder.ins().iconst(types::I64, tape_addr as i64);
        let tape_slot = builder.ins().iadd(tape_base, sp_val);
        builder.ins().store(MemFlags::new(), byte, tape_slot, 0);
        let one = builder.ins().iconst(types::I64, 1);
        let new_sp = builder.ins().iadd(sp_val, one);
        builder.def_var(sp, new_sp);
        let new_ptr = builder.ins().iadd(ptr_now, one);
        builder.def_var(cursor, new_ptr);
        builder.ins().jump(check_block, &[]);

        builder.switch_to_block(done_block);

        // Create a block for each state
        for stmt in &program.body {
            if let Stmt::Label(name) = stmt {
                label_blocks.insert(name, builder.create_block());
            }
        }

        // Emit instructions for state transitions
        let mut terminated = false;
        for (i, stmt) in program.body.iter().enumerate() {
            match stmt {
                Stmt::Label(name) => {
                    let target = label_blocks[name];
                    if !terminated {
                        builder.ins().jump(target, &[]);
                    }
                    builder.switch_to_block(target);
                    terminated = false;
                }
                Stmt::Jump(name) => {
                    if !terminated {
                        let target = label_blocks[name];
                        builder.ins().jump(target, &[]);
                        terminated = true;
                    }
                }
                Stmt::Push(byte) if !terminated => {
                    let sp_val = builder.use_var(sp);
                    let tape_base = builder.ins().iconst(types::I64, tape_addr as i64);
                    let addr = builder.ins().iadd(tape_base, sp_val);
                    let byte_val = builder.ins().iconst(types::I8, *byte as i64);
                    builder.ins().store(MemFlags::new(), byte_val, addr, 0);

                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().iadd(sp_val, one);
                    builder.def_var(sp, new_sp);
                }
                Stmt::Pop if !terminated => {
                    let sp_val = builder.use_var(sp);
                    let one = builder.ins().iconst(types::I64, 1);
                    let new_sp = builder.ins().isub(sp_val, one);
                    builder.def_var(sp, new_sp);
                }
                Stmt::Print if !terminated => {
                    let sp_val = builder.use_var(sp);
                    let one = builder.ins().iconst(types::I64, 1);
                    let top_index = builder.ins().isub(sp_val, one);
                    let tape_base = builder.ins().iconst(types::I64, tape_addr as i64);
                    let addr = builder.ins().iadd(tape_base, top_index);

                    let target = builder.ins().iconst(types::I64, write_char_addr as i64);
                    builder.ins().call_indirect(write_char_sig, target, &[addr]);
                }
                Stmt::Debug(_) if !terminated => {
                    let (str_addr, len) =
                        debug_addrs[i].expect("Debug statement missing precomputed address");
                    let target = builder.ins().iconst(types::I64, write_char_addr as i64);
                    for k in 0..len as u64 {
                        let byte_addr = builder.ins().iconst(types::I64, (str_addr + k) as i64);
                        builder
                            .ins()
                            .call_indirect(write_char_sig, target, &[byte_addr]);
                    }
                }
                // Dead code after a Jump and before the next Label --
                // intentionally not emitted.
                _ => {}
            }
        }

        // If the last block fell off the end of the program without
        // jumping anywhere, terminate it by exiting the process.
        if !terminated {
            let target = builder.ins().iconst(types::I64, exit_addr as i64);
            builder.ins().call_indirect(exit_sig, target, &[]);
            // Dead but required: Cranelift still needs a formal
            // terminator here even though `exit` never actually returns.
            builder.ins().return_(&[]);
        }

        builder.seal_all_blocks();
        builder.finalize();
    }

    println!("--- Generated Cranelift IR ---");
    let mut ir_text = String::new();
    cranelift_codegen::write::decorate_function(
        &mut LabelledWriter {
            label_blocks: &label_blocks,
        },
        &mut ir_text,
        &func,
    )
    .unwrap();
    println!("{ir_text}");
    println!("-------------------------------\n");

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
