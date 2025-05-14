use core::ffi::*;
use crate::{Op, Arg, Func, Compiler};
use crate::nob::*;

pub unsafe fn generate_arg(arg: Arg, output: *mut String_Builder) {
    match arg {
        Arg::AutoVar(index)      => sb_appendf(output, c!("vars[%zu]"), index - 1),
        Arg::Literal(value)      => sb_appendf(output, c!("%ld"), value),
        Arg::DataOffset(_offset) => todo!("DataOffset in js target"),
    };
}

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    sb_appendf(output, c!("function %s() {\n"), name);
    if auto_vars_count > 0 {
        sb_appendf(output, c!("    let vars = Array(%zu).fill(0);\n"), auto_vars_count);
    }
    for i in 0..body.len() {
        match (*body)[i] {
            Op::AutoAssign{index, arg} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::UnaryNot{..} => todo!(),
            Op::Add {index, lhs, rhs} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" + "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Sub {index, lhs, rhs} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" - "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Mul {index, lhs, rhs} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" * "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Less {index, lhs, rhs} => {
                sb_appendf(output, c!("    vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" < "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Funcall{result, name, args} => {
                sb_appendf(output, c!("    vars[%zu] = %s("), result - 1, name);
                for i in 0..args.count {
                    if i > 0 { sb_appendf(output, c!(", ")); }
                    generate_arg(*args.items.add(i), output);
                }
                sb_appendf(output, c!(");\n"));
            },
            Op::JmpIfNot{..} => todo!(),
            Op::Jmp{..} => todo!(),
        }
    }
    sb_appendf(output, c!("}\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    sb_appendf(output, c!("\"use strict\"\n"));
    generate_funcs(output, da_slice((*c).funcs));
    // TODO: Generate data section for js target
}
