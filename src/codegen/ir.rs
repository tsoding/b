use core::ffi::*;
use crate::nob::*;
use crate::{Op, Arg, Func, Compiler};

pub unsafe fn dump_arg(output: *mut String_Builder, arg: Arg) {
    match arg {
        Arg::External(name)     => sb_appendf(output, c!("External(%s)"), name),
        Arg::Ref(index)         => sb_appendf(output, c!("Ref(%zu)"), index),
        Arg::Literal(value)     => sb_appendf(output, c!("Literal(%ld)"), value),
        Arg::AutoVar(index)     => sb_appendf(output, c!("AutoVar(%zu)"), index),
        Arg::DataOffset(offset) => sb_appendf(output, c!("DataOffset(%zu)"), offset),
    };
}

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    sb_appendf(output, c!("%s(%zu):\n"), name, auto_vars_count);
    for i in 0..body.len() {
        sb_appendf(output, c!("%8zu"), i);
        match (*body)[i] {
            Op::Store{index, arg} => {
                sb_appendf(output, c!("    Store(%zu, "), index);
                dump_arg(output, arg);
                sb_appendf(output, c!(")\n"));
            }
            Op::ExternalAssign{..} => todo!(),
            Op::AutoAssign{index, arg} => {
                sb_appendf(output, c!("    AutoAssign(%zu, "), index);
                dump_arg(output, arg);
                sb_appendf(output, c!(")\n"));
            }
            Op::Negate{result, arg} => {
                sb_appendf(output, c!("    Negate(%zu, "), result);
                dump_arg(output, arg);
                sb_appendf(output, c!(")\n"));
            }
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("    UnaryNot(%zu, "), result);
                dump_arg(output, arg);
                sb_appendf(output, c!(")\n"));
            }
            Op::BitOr {index, lhs, rhs} => {
                sb_appendf(output, c!("    BitOr(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::BitAnd {index, lhs, rhs} => {
                sb_appendf(output, c!("    BitAnd(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::BitShl {index, lhs, rhs} => {
                sb_appendf(output, c!("    BitShl(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::BitShr {index, lhs, rhs} => {
                sb_appendf(output, c!("    BitShr(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::Add {index, lhs, rhs} => {
                sb_appendf(output, c!("    Add(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::Sub {index, lhs, rhs} => {
                sb_appendf(output, c!("    Sub(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::Mod {index, lhs, rhs} => {
                sb_appendf(output, c!("    Mod(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::Mul {index, lhs, rhs} => {
                sb_appendf(output, c!("    Mul(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::Less {index, lhs, rhs} => {
                sb_appendf(output, c!("    Less(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
            }
            Op::GreaterEqual {index, lhs, rhs} => {
                 sb_appendf(output, c!("    GTEq(%zu, "), index);
                dump_arg(output, lhs);
                sb_appendf(output, c!(", "));
                dump_arg(output, rhs);
                sb_appendf(output, c!(")\n"));
 
            }
            Op::Funcall{result, name, args} => {
                sb_appendf(output, c!("    Funcall(%zu, \"%s\""), result, name);
                for i in 0..args.count {
                    sb_appendf(output, c!(", "));
                    dump_arg(output, *args.items.add(i));
                }
                sb_appendf(output, c!(")\n"));
            }
            Op::JmpIfNot{addr, arg} => {
                sb_appendf(output, c!("    JmpIfNot(%zu, "), addr);
                dump_arg(output, arg);
                sb_appendf(output, c!(")\n"));
            }
            Op::Jmp{addr} => {
                sb_appendf(output, c!("    Jmp(%zu)\n"), addr);
            }
        }
    }
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    sb_appendf(output, c!("-- Functions --\n"));
    sb_appendf(output, c!("\n"));
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_extrns(output: *mut String_Builder, extrns: *const [*const c_char]) {
    sb_appendf(output, c!("\n"));
    sb_appendf(output, c!("-- External Symbols --\n\n"));
    for i in 0..extrns.len() {
        sb_appendf(output, c!("    %s\n"), (*extrns)[i]);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!("\n"));
        sb_appendf(output, c!("-- Data Section --\n"));
        sb_appendf(output, c!("\n"));
        sb_appendf(output, c!("    "));
        // TODO: display the IR Data Section in hex editor style
        for i in 0..data.len() {
            if i > 0 {
                sb_appendf(output, c!(","));
            }
            sb_appendf(output, c!("0x%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!("\n"));
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    generate_funcs(output, da_slice((*c).funcs));
    generate_extrns(output, da_slice((*c).extrns));
    generate_data_section(output, da_slice((*c).data));
}
