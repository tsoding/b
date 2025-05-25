use core::ffi::*;
use crate::{Op, Binop, OpWithLocation, Arg, Func, Compiler};
use crate::nob::*;
use crate::crust::libc::*;

pub unsafe fn generate_arg(arg: Arg, output: *mut String_Builder) {
    // TODO: convert all autovars to BigInt
    match arg {
        Arg::External(name)      => sb_appendf(output, c!("%s"), name),
        Arg::Ref(index)          => sb_appendf(output, c!("Number((new DataView(memory)).getBigUint64(vars[%zu], true))"), index - 1),
        Arg::AutoVar(index)      => sb_appendf(output, c!("vars[%zu]"), index - 1),
        Arg::Literal(value)      => sb_appendf(output, c!("%ld"), value),
        Arg::DataOffset(offset)  => sb_appendf(output, c!("%ld"), offset),
    };
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder) {
    sb_appendf(output, c!("function %s() {\n"), name);
    if auto_vars_count > 0 {
        sb_appendf(output, c!("    let vars = Array(%zu).fill(0);\n"), auto_vars_count);
    }
    assert!(auto_vars_count >= params_count);
    for i in 0..params_count {
        sb_appendf(output, c!("    vars[%zu] = arguments[%zu];\n"), i, i);
    }
    sb_appendf(output, c!("    let pc = 0;\n"));
    sb_appendf(output, c!("    while (pc < %zu) {\n"), body.len());
    sb_appendf(output, c!("        switch(pc) {\n"));
    for i in 0..body.len() {
        sb_appendf(output, c!("            case %zu: "), i);
        match (*body)[i].opcode {
            Op::Return {arg} => {
                sb_appendf(output, c!("return"));
                if let Some(arg) = arg {
                    sb_appendf(output, c!(" "));
                    generate_arg(arg, output);
                }
                sb_appendf(output, c!(";\n"));
            },
            Op::Store {index, arg} => {
                sb_appendf(output, c!("(new DataView(memory)).setBigUint64(vars[%zu], BigInt("), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!("), true);\n"));
            },
            Op::ExternalAssign {name, arg} => {
                sb_appendf(output, c!("%s = "), name);
                generate_arg(arg, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::AutoAssign{index, arg} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::Negate{result, arg} => {
                sb_appendf(output, c!("vars[%zu] = "), result - 1);
                sb_appendf(output, c!("-"));
                generate_arg(arg, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("vars[%zu] = "), result - 1);
                sb_appendf(output, c!("!"));
                generate_arg(arg, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::Binop{binop, index, lhs, rhs} => {
                match binop {
                    Binop::BitOr => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" | "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                    Binop::BitAnd => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" & "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                    Binop::BitShl => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" << "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                    Binop::BitShr => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" >> "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                    Binop::Plus  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" + "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::Minus  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" - "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::Mult  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" * "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::Mod  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" %% "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::Less  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" < "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::Equal  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" === "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::NotEqual  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" !== "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    }
                    Binop::GreaterEqual  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" >= "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                    Binop::LessEqual  => {
                        sb_appendf(output, c!("vars[%zu] = "), index - 1);
                        generate_arg(lhs, output);
                        sb_appendf(output, c!(" <= "));
                        generate_arg(rhs, output);
                        sb_appendf(output, c!(";\n"));
                    },
                }
            }
            Op::Funcall{result, name, args} => {
                sb_appendf(output, c!("vars[%zu] = %s("), result - 1, name);
                for i in 0..args.count {
                    if i > 0 { sb_appendf(output, c!(", ")); }
                    generate_arg(*args.items.add(i), output);
                }
                sb_appendf(output, c!(");\n"));
            },
            Op::JmpIfNot{addr, arg} => {
                sb_appendf(output, c!("if ("));
                generate_arg(arg, output);
                sb_appendf(output, c!(" == 0) { pc = %zu; continue; };\n"), addr);
            },
            Op::Jmp{addr} => {
                sb_appendf(output, c!("pc = %zu; continue;\n"), addr);
            },
        }
    }
    sb_appendf(output, c!("        }\n"));
    sb_appendf(output, c!("        break;\n"));
    sb_appendf(output, c!("    }\n"));
    sb_appendf(output, c!("}\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    sb_appendf(output, c!("const memory = new ArrayBuffer(%zu, { maxByteLength: 2**31-1 });\n"), data.len());
    if data.len() > 0 {
        sb_appendf(output, c!("(new Uint8Array(memory)).set(["));
        for i in 0..data.len() {
            sb_appendf(output, c!("0x%02X,"), (*data)[i] as i64);
        }
        sb_appendf(output, c!("])\n"));
    }
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [*const c_char]) {
    for i in 0..globals.len() {
        let name = (*globals)[i];
        sb_appendf(output, c!("let %s = 0;\n"), name);
    }
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    let template_cstr = c!(include_str!("html_js_template.tt"));
    let template_len = strlen(template_cstr);
    let generated = sv_from_cstr(c!("<<<GENERATED>>>"));
    let mut i = 0;
    while i < template_len {
        let prefix = sv_from_parts(template_cstr.add(i), template_len - i);
        if sv_starts_with(prefix, generated) {
            generate_data_section(output, da_slice((*c).data));
            generate_globals(output, da_slice((*c).globals));
            generate_funcs(output, da_slice((*c).funcs));
            i += generated.count;
        } else {
            da_append(output, *template_cstr.add(i));
            i += 1;
        }
    }
}
