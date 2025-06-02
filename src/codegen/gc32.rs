use core::ffi::*;
use crate::{Op, Binop, OpWithLocation, Arg, Func, Compiler, align_bytes};
use crate::nob::*;
use crate::crust::libc::*;
use crate::{missingf, Loc};

// Govno Core 32 memory map
// 0x00004000 - 0x0000FFFF      BSS uninitialized data
// 0x00030000                   Entry point
// 0x00700000 - 0x0070FFFF      BIOS (unused rn, but maybe i can update kasm to offset labels from assembly code
// 0x00FEFFFF - 0x00FE0000      Stack
// Our calling convention:
//   eax, ebx, ecx, edx, esi, egi, e8
// Return value is stored in eax

pub unsafe fn load_arg_to_reg(arg: Arg, reg: *const c_char, output: *mut String_Builder) {
    match arg {
        Arg::Deref(index) => {
            sb_appendf(output, c!("  sub %%ebp %zu\nlodd %%rbp %%%s\nadd %%ebp %zu\n"), index*1, reg, (index-1)*4);
            sb_appendf(output, c!("  lodd %%%s %%e9\nmov %%%s %%e9\n"), reg, reg)
        }
        Arg::RefAutoVar(index)  => sb_appendf(output, c!("  mov %s %%ebp\n  sub %s %zu\n"), reg, reg, index*4),
        Arg::RefExternal(name)  => sb_appendf(output, c!("  mov %%%s _%s\n"), reg, name),
        Arg::External(name)     => sb_appendf(output, c!("  mov %%e10 _%s\n  lodd %%e10 %%%s\n"), name, reg),
        Arg::AutoVar(index)     => sb_appendf(output, c!("  sub %%ebp %zu\n  lodd %%ebp %%%s\n  add %%ebp %zu\n"), index*4, reg, (index-1)*4),
        Arg::Literal(value)     => sb_appendf(output, c!("  mov %%%s %ld\n"), reg, value),
        Arg::DataOffset(offset) => sb_appendf(output, c!("  mov %%%s dat\n  add %%%s %zu\n"), reg, reg, offset),
    };
}

pub unsafe fn generate_function(name: *const c_char, name_loc: Loc, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder) {
    let stack_size = align_bytes(auto_vars_count*4, 16);
    sb_appendf(output, c!("_%s:\n"), name);
    sb_appendf(output, c!("  push %%ebp\n"));
    sb_appendf(output, c!("  mov %%ebp %%esp\n"));
    if stack_size > 0 {
        sb_appendf(output, c!("  sub %%esp %zu\n"), stack_size);
    }
    assert!(auto_vars_count >= params_count);
    const REGISTERS: *const[*const c_char] = &[c!("eax"), c!("ebx"), c!("ecx"), c!("edx"), c!("esi"), c!("egi"), c!("e8")];
    if params_count > REGISTERS.len() {
        missingf!(name_loc, c!("Too many parameters in function definition. We support only %zu but %zu were provided\n"), REGISTERS.len(), params_count);
    }
    for i in 0..params_count {
        let reg = (*REGISTERS)[i];
        sb_appendf(output, c!("  sub %%ebp %zu\n"), (i + 1)*4, reg);
        sb_appendf(output, c!("  stod %%ebp %%%s\n"), reg);
        sb_appendf(output, c!("  add %%ebp %zu\n"), i*4, reg);
    }
    for i in 0..body.len() {
        sb_appendf(output, c!(".op_%zu:\n"), i);
        let op = (*body)[i];
        match op.opcode {
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg_to_reg(arg, c!("eax"), output);
                }
                sb_appendf(output, c!("  mov %%esp %%ebp\n"));
                sb_appendf(output, c!("  pop %%ebp\n"));
                if strcmp(name, c!("main")) == 0 {
                  sb_appendf(output, c!("  hlt\n"));
                }
                else {
                  sb_appendf(output, c!("  ret\n"));
                }
            }
            Op::Store {index, arg} => {
                sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                sb_appendf(output, c!("  lodd %%ebp %%eax\n"), index*4);  // stod autoincrements
                sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4); // so it is (index-1)*4
                load_arg_to_reg(arg, c!("ebx"), output);
                sb_appendf(output, c!("  stod %%eax %%ebx\n"));
                sb_appendf(output, c!("  sub %%eax 4\n"));
            }
            Op::ExternalAssign{name, arg} => {
                load_arg_to_reg(arg, c!("eax"), output);
                sb_appendf(output, c!("  mov %%e10 _%s\n"), name);
                sb_appendf(output, c!("  stod %%e10 %%eax\n"), name);
            },
            Op::AutoAssign{index, arg} => {
                load_arg_to_reg(arg, c!("eax"), output);
                sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                sb_appendf(output, c!("  stod %%ebp %%eax\n"), index*4);
                sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
            },
            Op::Negate {result, arg} => {
                load_arg_to_reg(arg, c!("eax"), output);
                sb_appendf(output, c!("  neg %%eax\n"));
                sb_appendf(output, c!("  sub %%ebp %zu\n"), result*4);
                sb_appendf(output, c!("  stod %%ebp %%eax\n"), result*4);
                sb_appendf(output, c!("  add %%ebp %zu\n"), (result-1)*4);
            }
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("  xor %%ebx %%ebx\n"));
                load_arg_to_reg(arg, c!("eax"), output);
                sb_appendf(output, c!("  cmp %%eax 0\n"));
                sb_appendf(output, c!("  sub %%ebp %zu\n"), result*4);
                sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                sb_appendf(output, c!("  add %%ebp %zu\n"), (result-1)*4);
            },
            Op::Binop {binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  or %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::BitAnd => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  and %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::BitShl => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ecx"), output);
                        sb_appendf(output, c!("  salc %%eax\n")); // not implemented in gc32
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::BitShr => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ecx"), output);
                        sb_appendf(output, c!("  sarc %%eax\n")); // not implemented in gc32
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Plus => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  add %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Minus  => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  sub %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Mod => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  div %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%edx\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Div => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  div %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Mult => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  mul %%eax %%ebx\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%eax\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Less => { // a < b  ==  a-b < 0
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $02\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 1\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Greater => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $02\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 1\n"));
                        sb_appendf(output, c!("  not %%esi\n"));
                        sb_appendf(output, c!("  mov %%egi 1\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::Equal => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $04\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 1\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::NotEqual => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $04\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 2\n"));
                        sb_appendf(output, c!("  not %%esi\n"));
                        sb_appendf(output, c!("  mov %%egi 1\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::GreaterEqual => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $02\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 1\n"));
                        sb_appendf(output, c!("  mov %%egi $01\n"));
                        sb_appendf(output, c!("  xor %%esi %%egi\n"));
                        sb_appendf(output, c!("  nrm %%esi\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                    Binop::LessEqual => {
                        load_arg_to_reg(lhs, c!("eax"), output);
                        load_arg_to_reg(rhs, c!("ebx"), output);
                        sb_appendf(output, c!("  cmp %%eax %%ebx\n"));
                        sb_appendf(output, c!("  loadf\n"));
                        sb_appendf(output, c!("  mov %%egi $06\n"));
                        sb_appendf(output, c!("  mov %%esi %%eax\n"));
                        sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sar %%esi 1\n"));
                        // sb_appendf(output, c!("  not %%esi\n"));
                        // sb_appendf(output, c!("  mov %%egi 1\n"));
                        // sb_appendf(output, c!("  and %%esi %%egi\n"));
                        sb_appendf(output, c!("  sub %%ebp %zu\n"), index*4);
                        sb_appendf(output, c!("  stod %%ebp %%esi\n"));
                        sb_appendf(output, c!("  add %%ebp %zu\n"), (index-1)*4);
                    }
                }
            }
            Op::Funcall{result, name, args} => {
                if args.count > REGISTERS.len() {
                    missingf!(op.loc, c!("Too many function call arguments. We support only %d but %zu were provided\n"), REGISTERS.len(), args.count);
                }
                for i in 0..args.count {
                    let reg = (*REGISTERS)[i];
                    load_arg_to_reg(*args.items.add(i), reg, output);
                }
                sb_appendf(output, c!("  call _%s\n"), name);
                sb_appendf(output, c!("  sub %%ebp %zu\n"), result*4);
                sb_appendf(output, c!("  stod %%ebp %%eax\n"), result*4);
                sb_appendf(output, c!("  add %%ebp %zu\n"), (result-1)*4);
            },
            Op::JmpIfNot{addr, arg} => {
                load_arg_to_reg(arg, c!("eax"), output);
                sb_appendf(output, c!("  cmp %%eax 0\n"));
                sb_appendf(output, c!("  jz .op_%zu\n"), addr);
            },
            Op::Jmp{addr} => {
                sb_appendf(output, c!("  jmp .op_%zu\n"), addr);
            },
        }
    }
    sb_appendf(output, c!(".op_%zu:\n"), body.len());
    sb_appendf(output, c!("  xor %%eax %%eax\n"));
    sb_appendf(output, c!("  mov %%esp %%ebp\n"));
    sb_appendf(output, c!("  pop %%ebp\n"));
    sb_appendf(output, c!("  ret\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_extrns(output: *mut String_Builder, extrns: *const [*const c_char], funcs: *const [Func], globals: *const [*const c_char]) {
    'skip: for i in 0..extrns.len() {
        let name = (*extrns)[i];

        for j in 0..funcs.len() {
            let func = (*funcs)[j];
            if strcmp(func.name, name) == 0 {
                continue 'skip
            }
        }

        for j in 0..globals.len() {
            let global = (*globals)[j];
            if strcmp(global, name) == 0 {
                continue 'skip
            }
        }

        // Neccesary libgovn
        // write()
        sb_appendf(output, c!("_write:\n"));
        sb_appendf(output, c!("  dex %%ecx\n"));
        sb_appendf(output, c!(".loop:\n"));
        sb_appendf(output, c!("  lodb %%eax %%e9\n"));
        sb_appendf(output, c!("  push %%e9\n"));
        sb_appendf(output, c!("  int $02\n"));
        sb_appendf(output, c!("  loop .loop\n"));
        sb_appendf(output, c!("  ret\n"));

        if strcmp(name, c!("puts")) == 0 {
          sb_appendf(output, c!("_puts:\n"));
          sb_appendf(output, c!("  lodb %%eax %%e9\n"));
          sb_appendf(output, c!("  cmp %%e9 0\n"));
          sb_appendf(output, c!("  re\n"));
          sb_appendf(output, c!("  push %%e9\n"));
          sb_appendf(output, c!("  int 2\n"));
          sb_appendf(output, c!("  jmp _puts\n"));
        }
        else if strcmp(name, c!("strcpy")) == 0 {
          sb_appendf(output, c!("_strcpy:\n"));
          sb_appendf(output, c!("  lodb %%ebx %%e9\n"));
          sb_appendf(output, c!("  cmp %%e9 0\n"));
          sb_appendf(output, c!("  re\n"));
          sb_appendf(output, c!("  stob %%eax %%e9\n"));
          sb_appendf(output, c!("  jmp _strcpy\n"));
        }
        else if strcmp(name, c!("puti")) == 0 {
          sb_appendf(output, c!("_puti:\n"));
          sb_appendf(output, c!("  mov %%e9 .puti_buf\n"));
          sb_appendf(output, c!("  add %%e9 10\n"));
          sb_appendf(output, c!(".loop:\n"));
          sb_appendf(output, c!("  div %%eax 10\n"));
          sb_appendf(output, c!("  add %%edx 48\n"));
          sb_appendf(output, c!("  stob %%e9 %%edx\n"));
          sb_appendf(output, c!("  sub %%e9 2\n"));
          sb_appendf(output, c!("  cmp %%eax $00\n"));
          sb_appendf(output, c!("  jne .loop\n"));
          sb_appendf(output, c!("  mov %%eax .puti_buf\n"));
          sb_appendf(output, c!("  mov %%ecx 11\n"));
          sb_appendf(output, c!("  call _write\n"));
          sb_appendf(output, c!("  call _puti_local_clr\n"));
          sb_appendf(output, c!("  ret\n"));
          sb_appendf(output, c!(".puti_buf: reserve 11 bytes\n"));
          sb_appendf(output, c!("_puti_local_clr:\n"));
          sb_appendf(output, c!("  mov %%esi _puti.puti_buf\n"));
          sb_appendf(output, c!("  mov %%eax $00\n"));
          sb_appendf(output, c!("  mov %%ecx 10\n"));
          sb_appendf(output, c!(".loop:\n"));
          sb_appendf(output, c!("  stob %%esi %%eax\n"));
          sb_appendf(output, c!("  loop .loop\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("putchar")) == 0 {
          sb_appendf(output, c!("_putchar:\n"));
          sb_appendf(output, c!("  push %%eax\n"));
          sb_appendf(output, c!("  int 2\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("getchar")) == 0 {
          sb_appendf(output, c!("_getchar:\n"));
          sb_appendf(output, c!("  int 1\n"));
          sb_appendf(output, c!("  pop %%eax\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("sleep")) == 0 {
          sb_appendf(output, c!("_sleep:\n"));
          sb_appendf(output, c!("  mov %%eax %%edx\n"));
          sb_appendf(output, c!("  int $22\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("char")) == 0 {
          sb_appendf(output, c!("_char:\n"));
          sb_appendf(output, c!("  add %%eax %%ebx\n"));
          sb_appendf(output, c!("  lodb %%eax %%e9\n"));
          sb_appendf(output, c!("  mov %%eax %%e9\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("lchar")) == 0 {
          sb_appendf(output, c!("_lchar:\n"));
          sb_appendf(output, c!("  add %%eax %%ebx\n"));
          sb_appendf(output, c!("  stob %%eax %%ecx\n"));
          sb_appendf(output, c!("  xor %%eax %%eax\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else if strcmp(name, c!("render")) == 0 {
          sb_appendf(output, c!("_render:\n"));
          sb_appendf(output, c!("  int $11\n"));
          sb_appendf(output, c!("  ret\n"));
        }
        else {
          printf(c!("Can't find `%s` in libgovn\n"), name);
        }
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [*const c_char]) {
    for i in 0..globals.len() {
        let name = (*globals)[i];
        sb_appendf(output, c!("_%s: reserve 4 bytes\n"), name);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!("dat: bytes "));
        for i in 0..data.len() {
            if i > 0 {
                sb_appendf(output, c!(" "));
            }
            sb_appendf(output, c!("$%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    sb_appendf(output, c!("jmp _main\n"));
    generate_funcs(output, da_slice((*c).funcs));
    generate_extrns(output, da_slice((*c).extrns), da_slice((*c).funcs), da_slice((*c).globals));
    generate_data_section(output, da_slice((*c).data));
    generate_globals(output, da_slice((*c).globals));
}
