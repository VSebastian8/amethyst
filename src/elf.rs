/// Replaces a traditional linker (since we only need libc for printing)
pub const WRITE_CHAR: [u8; 21] = [
    0x48, 0x89, 0xfe, // mov rsi, rdi
    0xbf, 0x01, 0x00, 0x00, 0x00, // mov edi, 1
    0xba, 0x01, 0x00, 0x00, 0x00, // mov edx, 1
    0xb8, 0x01, 0x00, 0x00, 0x00, // mov eax, 1
    0x0f, 0x05, // syscall
    0xc3, // ret
];
pub const EXIT_PROCESS: [u8; 12] = [
    0xbf, 0x00, 0x00, 0x00, 0x00, // mov edi, 0
    0xb8, 0x3c, 0x00, 0x00, 0x00, // mov eax, 60
    0x0f, 0x05, // syscall
];
pub const BASE_ADDR: u64 = 0x400000;
pub const HEADER_SIZE: u64 = 64 + 56;

pub fn start_stub(main_addr: u64) -> [u8; 17] {
    let a = main_addr.to_le_bytes();
    [
        0x48, 0x8b, 0x7c, 0x24, 0x10, // mov rdi, [rsp+16]
        0x48, 0xb8, // movabs rax, imm64
        a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], 0xff, 0xe0, // jmp rax
    ]
}

pub fn build_executable(code: &[u8], entry_offset: u64) -> Vec<u8> {
    let mut file = Vec::with_capacity(HEADER_SIZE as usize + code.len());

    let entry_vaddr = BASE_ADDR + HEADER_SIZE + entry_offset;
    let total_size = HEADER_SIZE + code.len() as u64;

    write_elf_header(&mut file, entry_vaddr);
    write_program_header(&mut file, total_size);
    file.extend_from_slice(code);

    debug_assert_eq!(file.len() as u64, total_size);
    file
}

fn write_elf_header(out: &mut Vec<u8>, entry_vaddr: u64) {
    // e_ident: magic number + a few fixed identification bytes.
    out.extend_from_slice(&[0x7f, b'E', b'L', b'F']);
    out.push(2); // EI_CLASS: 2 = 64-bit
    out.push(1); // EI_DATA: 1 = little-endian
    out.push(1); // EI_VERSION: must be 1
    out.push(0); // EI_OSABI: 0 = System V (works fine for a static Linux binary)
    out.extend_from_slice(&[0u8; 8]); // EI_ABIVERSION + padding, unused

    out.extend_from_slice(&2u16.to_le_bytes()); // e_type: ET_EXEC (a plain static executable)
    out.extend_from_slice(&0x3Eu16.to_le_bytes()); // e_machine: EM_X86_64
    out.extend_from_slice(&1u32.to_le_bytes()); // e_version: must be 1
    out.extend_from_slice(&entry_vaddr.to_le_bytes()); // e_entry: where execution starts
    out.extend_from_slice(&64u64.to_le_bytes()); // e_phoff: program header starts right after this header
    out.extend_from_slice(&0u64.to_le_bytes()); // e_shoff: no section headers -- we don't need them to run
    out.extend_from_slice(&0u32.to_le_bytes()); // e_flags: unused for x86-64
    out.extend_from_slice(&64u16.to_le_bytes()); // e_ehsize: this header's own size
    out.extend_from_slice(&56u16.to_le_bytes()); // e_phentsize: size of one program header entry
    out.extend_from_slice(&1u16.to_le_bytes()); // e_phnum: exactly one segment
    out.extend_from_slice(&0u16.to_le_bytes()); // e_shentsize: no section headers
    out.extend_from_slice(&0u16.to_le_bytes()); // e_shnum
    out.extend_from_slice(&0u16.to_le_bytes()); // e_shstrndx

    debug_assert_eq!(out.len(), 64);
}

fn write_program_header(out: &mut Vec<u8>, total_size: u64) {
    const PT_LOAD: u32 = 1;
    const PF_X: u32 = 1; // executable
    const PF_W: u32 = 2; // writable
    const PF_R: u32 = 4; // readable

    out.extend_from_slice(&PT_LOAD.to_le_bytes());
    out.extend_from_slice(&(PF_R | PF_W | PF_X).to_le_bytes()); // p_flags
    out.extend_from_slice(&0u64.to_le_bytes()); // p_offset: segment starts at file offset 0
    out.extend_from_slice(&BASE_ADDR.to_le_bytes()); // p_vaddr
    out.extend_from_slice(&BASE_ADDR.to_le_bytes()); // p_paddr: unused on Linux, conventionally == p_vaddr
    out.extend_from_slice(&total_size.to_le_bytes()); // p_filesz: bytes present in the file
    out.extend_from_slice(&total_size.to_le_bytes()); // p_memsz: same -- no zero-filled bss growth needed
    out.extend_from_slice(&0x1000u64.to_le_bytes()); // p_align: page alignment

    debug_assert_eq!(out.len(), 120);
}
