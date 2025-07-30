use core::ffi::*;
use core::cmp;
use crate::ir::*;
use crate::nob::*;
use crate::targets::Os;
use crate::crust::libc::*;
use crate::lexer::Loc;

pub unsafe fn align_bytes(bytes: usize, alignment: usize) -> usize {
    let rem = bytes%alignment;
    if rem > 0 {
        bytes + alignment - rem
    } else {
        bytes
    }
}

pub unsafe fn call_arg(arg: Arg, output: *mut String_Builder, os: Os) {
    match arg {
        Arg::RefExternal(name) | Arg::External(name) => {
            match os {
                Os::Linux | Os::Windows => sb_appendf(output, c!("    call %s\n"), name),
                Os::Darwin              => sb_appendf(output, c!("    call _%s\n"), name),
            }
        }
        arg => {
            load_arg_to_reg(arg, c!("rax"), output, os);
            sb_appendf(output, c!("    call *%%rax\n"))
        }
    };
}

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char,output: *mut String_Builder, os: Os) {
    match arg {
        Arg::Deref(index) => {
            sb_appendf(output, c!("    movq -%zu(%%rbp), %%%s\n"), index * 8, reg);
            sb_appendf(output, c!("    movq (%%%s), %%%s\n"), reg, reg)
        }
        Arg::RefAutoVar(index)  => sb_appendf(output, c!("    leaq -%zu(%%rbp), %%%s\n"), index * 8, reg),
        Arg::RefExternal(name)  => match os {
            Os::Linux | Os::Windows => sb_appendf(output, c!("    leaq %s(%%rip), %%%s\n"), name, reg),
            Os::Darwin              => sb_appendf(output, c!("    mov _%s@GOTPCREL(%%rip), %%%s\n"), name, reg),
        },
        Arg::External(name)     => match os {
            Os::Linux | Os::Windows => sb_appendf(output, c!("    movq %s(%%rip), %%%s\n"), name, reg),
            Os::Darwin              => sb_appendf(output, c!("    movq _%s(%%rip), %%%s\n"), name, reg),
        },
        Arg::AutoVar(index)     => sb_appendf(output, c!("    movq -%zu(%%rbp), %%%s\n"), index * 8, reg),
        Arg::Literal(value)     => sb_appendf(output, c!("    movq $%lld, %%%s\n"), value, reg),
        Arg::DataOffset(offset) => {sb_appendf(output, c!("    leaq dat+%zu(%%rip), %%%s\n"), offset, reg)},
        Arg::Bogus => unreachable!("bogus-amogus"),
    };
}

pub unsafe fn generate_function(name: *const c_char, name_loc: Loc, func_index: usize, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], debug: bool, output: *mut String_Builder, os: Os) {
    let stack_size = align_bytes(auto_vars_count * 8, 16);
    match os {
        Os::Linux | Os::Windows => {
            sb_appendf(output, c!(".global %s\n"), name);
            sb_appendf(output, c!(".p2align 4, 0x90\n"));
            sb_appendf(output, c!("%s:\n"), name);
        }
        Os::Darwin => {
            sb_appendf(output, c!(".global _%s\n"), name);
            sb_appendf(output, c!(".p2align 4, 0x90\n"));
            sb_appendf(output, c!("_%s:\n"), name);
        }
    }

    if debug {
        sb_appendf(output, c!("    .file %lld \"%s\"\n"), func_index, name_loc.input_path);
        // we need to place line information directly after the label, before any instrucitons
        // ideally pointing to the first statement instead of the function name
        if body.len() > 0 {
            sb_appendf(output, c!("    .loc %lld %lld\n"), func_index, (*body)[0].loc.line_number);
        } else {
            sb_appendf(output, c!("    .loc %lld %lld\n"), func_index, name_loc.line_number);
        }
    }

    sb_appendf(output, c!("    pushq %%rbp\n"));
    sb_appendf(output, c!("    movq %%rsp, %%rbp\n"));
    if stack_size > 0 {
        sb_appendf(output, c!("    subq $%zu, %%rsp\n"), stack_size);
    }
    assert!(auto_vars_count >= params_count);
        let registers: *const[*const c_char] = match os {
        Os::Linux | Os::Darwin => &[c!("rdi"), c!("rsi"), c!("rdx"), c!("rcx"), c!("r8"), c!("r9")],
        Os::Windows => &[c!("rcx"), c!("rdx"), c!("r8"), c!("r9")], // https://en.wikipedia.org/wiki/X86_calling_conventions#Microsoft_x64_calling_convention
    };

    let mut i = 0;
    while i < cmp::min(params_count, registers.len()) {
        let reg = (*registers)[i];
        sb_appendf(output, c!("    movq %%%s, -%zu(%%rbp)\n"), reg, (i + 1) * 8);
        i += 1;
    }
    for j in i..params_count {
        match os {
            Os::Linux | Os::Darwin => sb_appendf(output, c!("    movq %zu(%%rbp), %%rax\n"), ((j - i) + 2)*8),
            Os::Windows => sb_appendf(output, c!("    movq %zu(%%rbp), %%rax\n"), ((j - i) + 6)*8),
        };
        sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), (j + 1)*8);
    }

    for i in 0..body.len() {
        let op = (*body)[i];

        if debug {
            // location info of the first op has already been pushed
            if i > 0 {
                sb_appendf(output, c!("    .loc %lld %lld\n"), func_index, op.loc.line_number);
            }
        }

        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::Return { arg } => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("rax"), output, os);
                }
                sb_appendf(output, c!("    movq %%rbp, %%rsp\n"));
                sb_appendf(output, c!("    popq %%rbp\n"));
                sb_appendf(output, c!("    ret\n"));
            }
            Op::Store { index, arg } => {
                sb_appendf(output, c!("    movq -%zu(%%rbp), %%rax\n"), index * 8);
                load_arg_to_reg(arg, c!("rcx"), output, os);
                sb_appendf(output, c!("    movq %%rcx, (%%rax)\n"));
            }
            Op::ExternalAssign { name, arg } => {
                load_arg_to_reg(arg, c!("rax"), output, os);
                match os {
                    Os::Linux | Os::Windows => sb_appendf(output, c!("    movq %%rax, %s(%%rip)\n"), name),
                    Os::Darwin              => sb_appendf(output, c!("    movq %%rax, _%s(%%rip)\n"), name),
                };
            }
            Op::AutoAssign { index, arg } => {
                load_arg_to_reg(arg, c!("rax"), output, os);
                sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), index * 8);
            }
            Op::Negate { result, arg } => {
                load_arg_to_reg(arg, c!("rax"), output, os);
                sb_appendf(output, c!("    negq %%rax\n"));
                sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), result * 8);
            }
            Op::UnaryNot { result, arg } => {
                sb_appendf(output, c!("    xorq %%rcx, %%rcx\n"));
                load_arg_to_reg(arg, c!("rax"), output, os);
                sb_appendf(output, c!("    testq %%rax, %%rax\n"));
                sb_appendf(output, c!("    setz %%cl\n"));
                sb_appendf(output, c!("    movq %%rcx, -%zu(%%rbp)\n"), result * 8);
            }
            Op::Binop {binop, index, lhs, rhs} => {
                load_arg_to_reg(lhs, c!("rax"), output, os);
                load_arg_to_reg(rhs, c!("rcx"), output, os);
                match binop {
                    Binop::BitOr => { sb_appendf(output, c!("    orq %%rcx, %%rax\n")); }
                    Binop::BitAnd => { sb_appendf(output, c!("    andq %%rcx, %%rax\n")); }
                    Binop::BitShl => {
                        load_arg_to_reg(rhs, c!("rcx"), output, os);
                        sb_appendf(output, c!("    shlq %%cl, %%rax\n"));
                    }
                    Binop::BitShr => {
                        load_arg_to_reg(rhs, c!("rcx"), output, os);
                        sb_appendf(output, c!("    shrq %%cl, %%rax\n"));
                    }
                    Binop::Plus => { sb_appendf(output, c!("    addq %%rcx, %%rax\n")); }
                    Binop::Minus => { sb_appendf(output, c!("    subq %%rcx, %%rax\n")); }
                    Binop::Mod => {
                        sb_appendf(output, c!("    cqto\n"));
                        sb_appendf(output, c!("    idivq %%rcx\n"));
                        sb_appendf(output, c!("    movq %%rdx, -%zu(%%rbp)\n"), index * 8);
                        continue;
                    }
                    Binop::Div => {
                        sb_appendf(output, c!("    cqto\n"));
                        sb_appendf(output, c!("    idivq %%rcx\n"));
                    }
                    Binop::Mult => {
                        sb_appendf(output, c!("    imulq %%rcx, %%rax\n"));
                    }
                    _ => {
                        sb_appendf(output, c!("    xorq %%rdx, %%rdx\n"));
                        sb_appendf(output, c!("    cmpq %%rcx, %%rax\n"));
                        match binop {
                            Binop::Less => sb_appendf(output, c!("    setl %%dl\n")),
                            Binop::Greater => sb_appendf(output, c!("    setg %%dl\n")),
                            Binop::Equal => sb_appendf(output, c!("    sete %%dl\n")),
                            Binop::NotEqual => sb_appendf(output, c!("    setne %%dl\n")),
                            Binop::GreaterEqual => sb_appendf(output, c!("    setge %%dl\n")),
                            Binop::LessEqual => sb_appendf(output, c!("    setle %%dl\n")),
                            _ => unreachable!(),
                        };
                        sb_appendf(output, c!("    movq %%rdx, -%zu(%%rbp)\n"), index * 8);
                        continue;
                    }
                }
                sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), index * 8);
            }
            Op::Funcall { result, fun, args } => {
                let reg_args_count = cmp::min(args.count, registers.len());
                for i in 0..reg_args_count {
                    let reg = (*registers)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output, os);
                }

                let stack_args_count = args.count - reg_args_count;
                let stack_args_size = align_bytes(stack_args_count * 8, 16);
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    subq $%zu, %%rsp\n"), stack_args_size);
                    for i in 0..stack_args_count {
                        load_arg_to_reg(*args.items.add(reg_args_count + i), c!("rax"), output, os);
                        sb_appendf(output, c!("    movq %%rax, %zu(%%rsp)\n"), i * 8);
                    }
                }
                match os {
                    Os::Linux | Os::Darwin => {
                        sb_appendf(output, c!("    movb $0, %%al\n")); // x86_64 Linux ABI passes the amount of
                                                                       // floating point args via al. Since B
                                                                       // does not distinguish regular and
                                                                       // variadic functions we set al to 0 just
                                                                       // in case.
                        call_arg(fun, output, os);
                    }
                    Os::Windows => {
                        // allocate 32 bytes for "shadow space"
                        // it must be allocated at the top of the stack after all arguments are pushed
                        // so we can't allocate it at function prologue
                        sb_appendf(output, c!("    subq $32, %%rsp\n"));
                        call_arg(fun, output, os);
                        sb_appendf(output, c!("    addq $32, %%rsp\n"));
                    }
                }
                if stack_args_count > 0 {
                    sb_appendf(output, c!("    addq $%zu, %%rsp\n"), stack_args_size);
                }
                sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), result * 8);
            }
            Op::Asm { stmts } => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    sb_appendf(output, c!("    %s\n"), stmt.line);
                }
            }
            //All labels are global in GAS, so we need to namespace them.
            Op::Label { label } => {
                match os {
                    Os::Linux | Os::Windows => sb_appendf(output, c!(".L%s_label_%zu:\n"), name, label),
                    Os::Darwin              => sb_appendf(output, c!("L%s_label_%zu:\n"), name, label),
                };
            }
            Op::JmpLabel { label } => {
                match os {
                    Os::Linux | Os::Windows => sb_appendf(output, c!("    jmp .L%s_label_%zu\n"), name, label),
                    Os::Darwin              => sb_appendf(output, c!("    jmp L%s_label_%zu\n"), name, label),
                };
            }
            Op::JmpIfNotLabel { label, arg } => {
                load_arg_to_reg(arg, c!("rax"), output, os);
                sb_appendf(output, c!("    testq %%rax, %%rax\n"));
                match os {
                    Os::Linux | Os::Windows => sb_appendf(output, c!("    jz .L%s_label_%zu\n"), name, label),
                    Os::Darwin              => sb_appendf(output, c!("    jz L%s_label_%zu\n"), name, label),
                };
            }
            Op::Index {result, arg, offset} => {
                load_arg_to_reg(arg, c!("rax"), output, os);
                load_arg_to_reg(offset, c!("rcx"), output, os);
                sb_appendf(output, c!("    leaq (%%rax, %%rcx, 8), %%rax\n"));
                sb_appendf(output, c!("    movq %%rax, -%zu(%%rbp)\n"), result * 8);
            },
        }
    }
    sb_appendf(output, c!("    movq $0, %%rax\n"));
    sb_appendf(output, c!("    movq %%rbp, %%rsp\n"));
    sb_appendf(output, c!("    popq %%rbp\n"));
    sb_appendf(output, c!("    ret\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], debug: bool, os: Os) {
    for i in 0..funcs.len() {
        let func = (*funcs)[i];
        generate_function(func.name, func.name_loc, i, func.params_count, func.auto_vars_count, da_slice(func.body), debug, output, os);
    }
}

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc], os: Os) {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        match os {
            Os::Linux | Os::Windows => {
                sb_appendf(output, c!(".global %s\n"), asm_func.name);
                sb_appendf(output, c!(".p2align 4, 0x90\n"));
                sb_appendf(output, c!("%s:\n"), asm_func.name);
            }
            Os::Darwin => {
                sb_appendf(output, c!(".global _%s\n"), asm_func.name);
                sb_appendf(output, c!(".p2align 4, 0x90\n"));
                sb_appendf(output, c!("_%s:\n"), asm_func.name);
            }
        }
        for j in 0..asm_func.body.count {
            let stmt = *asm_func.body.items.add(j);
            sb_appendf(output, c!("    %s\n"), stmt.line);
        }
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [Global], os: Os) {
    for i in 0..globals.len() {
        let global = (*globals)[i];
        match os {
            Os::Linux | Os::Windows => {
                sb_appendf(output, c!(".global %s\n"), global.name);
                sb_appendf(output, c!(".p2align 3\n"));
                sb_appendf(output, c!("%s:\n"), global.name);
            }
            Os::Darwin => {
                sb_appendf(output, c!(".global _%s\n"), global.name);
                sb_appendf(output, c!(".p2align 3\n"));
                sb_appendf(output, c!("_%s:\n"), global.name);
            }
        }

        if global.is_vec {
            sb_appendf(output, c!(".quad . + 8\n"));
        }

        if global.values.count > 0 {
            sb_appendf(output, c!(".quad "));
            for j in 0..global.values.count {
                if j > 0 {
                    sb_appendf(output, c!(","));
                }
                match *global.values.items.add(j) {
                    ImmediateValue::Literal(lit)       => sb_appendf(output, c!("0x%llX"), lit),
                    ImmediateValue::Name(name)         => match os {
                        Os::Linux | Os::Windows => sb_appendf(output, c!("%s"), name),
                        Os::Darwin              => sb_appendf(output, c!("_%s"), name),
                    }
                    ImmediateValue::DataOffset(offset) => sb_appendf(output, c!("dat+%zu"), offset),
                };
            }
            sb_appendf(output, c!("\n"));
        }
        if global.values.count < global.minimum_size {
            let reserved_qwords = global.minimum_size - global.values.count;
            if reserved_qwords > 0 {
                sb_appendf(output, c!(".space %zu\n"), reserved_qwords * 8);
            }
        }
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!("dat: .byte "));
        for i in 0..data.len() {
            if i > 0 {
                sb_appendf(output, c!(","));
            }
            sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_program(
    // Inputs
    p: *const Program, program_path: *const c_char, garbage_base: *const c_char,
    linker: *const [*const c_char], run_args: *const [*const c_char], os: Os,
    nostdlib: bool, debug: bool, nobuild: bool, run: bool,
    // Temporaries
    output: *mut String_Builder, cmd: *mut Cmd,
) -> Option<()> {
    if !nobuild {
        match os {
            Os::Darwin => sb_appendf(output, c!(".text\n")),
            Os::Linux | Os::Windows => sb_appendf(output, c!(".section .text\n")),
        };
        generate_funcs(output, da_slice((*p).funcs), debug, os);
        generate_asm_funcs(output, da_slice((*p).asm_funcs), os);
        match os {
            Os::Darwin => sb_appendf(output, c!(".data\n")),
            Os::Linux | Os::Windows => sb_appendf(output, c!(".section .data\n")),
        };
        generate_data_section(output, da_slice((*p).data));
        generate_globals(output, da_slice((*p).globals), os);

        let output_asm_path = temp_sprintf(c!("%s.s"), garbage_base);
        write_entire_file(output_asm_path, (*output).items as *const c_void, (*output).count)?;
        log(Log_Level::INFO, c!("generated %s"), output_asm_path);

        match os {
            Os::Darwin => {
                if !(cfg!(target_os = "macos")) {
                    // TODO: think how to approach cross-compilation
                    log(Log_Level::ERROR, c!("Cross-compilation of darwin is not supported"));
                    return None;
                }

                let (gas, cc) = (c!("as"), c!("cc"));

                let output_obj_path = temp_sprintf(c!("%s.o"), program_path);
                cmd_append! {
                    cmd,
                    gas, c!("-arch"), c!("x86_64"), c!("-o"), output_obj_path, output_asm_path,
                }
                if !cmd_run_sync_and_reset(cmd) { return None; }

                cmd_append! {
                    cmd,
                    cc, c!("-arch"), c!("x86_64"), c!("-o"), program_path, output_obj_path,
                }
                if nostdlib {
                    cmd_append!(cmd, c!("-nostdlib"));
                }
                da_append_many(cmd, linker);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
            Os::Linux => {
                if !(cfg!(target_arch = "x86_64") && cfg!(target_os = "linux")) {
                    // TODO: think how to approach cross-compilation
                    log(Log_Level::ERROR, c!("Cross-compilation of x86_64 linux is not supported for now"));
                    return None;
                }

                let output_obj_path = temp_sprintf(c!("%s.o"), garbage_base);
                cmd_append! {
                    cmd,
                    c!("as"), output_asm_path, c!("-o"), output_obj_path,
                }
                if !cmd_run_sync_and_reset(cmd) { return None; }

                cmd_append! {
                    cmd,
                    c!("cc"), c!("-no-pie"), c!("-o"), program_path, output_obj_path,
                }
                if nostdlib {
                    cmd_append!(cmd, c!("-nostdlib"));
                }
                da_append_many(cmd, linker);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
            Os::Windows => {
                let output_obj_path = temp_sprintf(c!("%s.o"), garbage_base);
                cmd_append! {
                    cmd,
                    c!("as"), output_asm_path, c!("-o"), output_obj_path,
                }
                if !cmd_run_sync_and_reset(cmd) { return None; }

                cmd_append! {
                    cmd,
                    c!("x86_64-w64-mingw32-gcc"), c!("-no-pie"), c!("-o"), program_path, output_obj_path,
                }
                if nostdlib {
                    cmd_append!(cmd, c!("-nostdlib"));
                }
                da_append_many(cmd, linker);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
        }
    }

    if run {
        match os {
            Os::Linux => {
                // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
                let run_path: *const c_char;
                if (strchr(program_path, '/' as c_int)).is_null() {
                    run_path = temp_sprintf(c!("./%s"), program_path);
                } else {
                    run_path = program_path;
                }

                cmd_append! {cmd, run_path}
                da_append_many(cmd, run_args);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
            Os::Windows => {
                // TODO: document that you may need wine as a system package to cross-run gas-x86_64-windows
                if !cfg!(target_os = "windows") {
                    cmd_append! {
                        cmd,
                        c!("wine"),
                    }
                }

                cmd_append! {cmd, program_path}
                da_append_many(cmd, run_args);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
            Os::Darwin => {
                if !cfg!(target_os = "macos") {
                    log(Log_Level::ERROR, c!("This runner is only for macOS, but the current target is not macOS."));
                    return None;
                }

                // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Darwin. It has to be `./program`.
                let run_path: *const c_char;
                if (strchr(program_path, '/' as c_int)).is_null() {
                    run_path = temp_sprintf(c!("./%s"), program_path);
                } else {
                    run_path = program_path;
                }

                cmd_append! {cmd, run_path}
                da_append_many(cmd, run_args);
                if !cmd_run_sync_and_reset(cmd) { return None; }
            }
        }
    }

    Some(())
}