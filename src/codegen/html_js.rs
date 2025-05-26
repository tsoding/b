use core::ffi::*;
use crate::{Op, Binop, OpWithLocation, Arg, Func, Compiler};
use crate::nob::*;
use crate::crust::libc::*;

pub unsafe fn generate_arg(arg: Arg, output: *mut String_Builder) {
    match arg {
        Arg::External(name)      => sb_appendf(output, c!("memory.get_global(\"%s\")"), name),
        Arg::Ref(index)          => sb_appendf(output, c!("memory.get(memory.get_local(%zu))"), index - 1),
        Arg::AutoVar(index)      => sb_appendf(output, c!("memory.get_local(%zu)"), index - 1),
        Arg::Literal(value)      => sb_appendf(output, c!("%ldn"), value),
        Arg::DataOffset(offset)  => sb_appendf(output, c!("%ldn"), offset),
    };
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder) {
    sb_appendf(output, c!("function %s() {\n"), name);
    sb_appendf(output, c!("    memory.push_stack(%zu);\n"), auto_vars_count);
    assert!(auto_vars_count >= params_count);
    for i in 0..params_count {
        sb_appendf(output, c!("    memory.set_local(%zu, arguments[%zu]);\n"), i, i);
    }
    sb_appendf(output, c!("    let pc = 0;\n"));
    sb_appendf(output, c!("    while (true) {\n"), body.len());
    sb_appendf(output, c!("        switch(pc) {\n"));
    for i in 0..body.len() {
        sb_appendf(output, c!("            case %zu: "), i);
        match (*body)[i].opcode {
            Op::Return {arg} => {
                sb_appendf(output, c!("memory.pop_stack(); "));
                sb_appendf(output, c!("return"));
                if let Some(arg) = arg {
                    sb_appendf(output, c!(" "));
                    generate_arg(arg, output);
                }
                sb_appendf(output, c!(";\n"));
            },
            Op::Store {index, arg} => {
                sb_appendf(output, c!("memory.set(memory.get_local(%zu), "), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!(");\n"));
            },
            Op::ExternalAssign {name, arg} => {
                sb_appendf(output, c!("memory.set_global(\"%s\", "), name);
                generate_arg(arg, output);
                sb_appendf(output, c!(");\n"));
            }
            Op::AutoAssign{index, arg} => {
                sb_appendf(output, c!("memory.set_local(%zu, "), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!(");\n"));
            },
            Op::Negate{result, arg} => {
                sb_appendf(output, c!("memory.set_local(%zu, "), result - 1);
                sb_appendf(output, c!("-"));
                generate_arg(arg, output);
                sb_appendf(output, c!(");\n"));
            }
            Op::UnaryNot{result, arg} => {
                sb_appendf(output, c!("memory.set_local(%zu, "), result - 1);
                sb_appendf(output, c!("!"));
                generate_arg(arg, output);
                sb_appendf(output, c!(");\n"));
            },
            Op::Binop{binop, index, lhs, rhs} => {
                sb_appendf(output, c!("memory.set_local(%zu, "), index - 1);
                generate_arg(lhs, output);
                match binop {
                    Binop::BitOr        => { sb_appendf(output, c!(" | "));  }
                    Binop::BitAnd       => { sb_appendf(output, c!(" & "));  }
                    Binop::BitShl       => { sb_appendf(output, c!(" << ")); }
                    Binop::BitShr       => { sb_appendf(output, c!(" >> ")); }
                    Binop::Plus         => { sb_appendf(output, c!(" + "));  }
                    Binop::Minus        => { sb_appendf(output, c!(" - "));  }
                    Binop::Mult         => { sb_appendf(output, c!(" * "));  }
                    Binop::Mod          => { sb_appendf(output, c!(" %% ")); }
                    Binop::Div          => { sb_appendf(output, c!(" / ")); }
                    Binop::Less         => { sb_appendf(output, c!(" < "));   }
                    Binop::Equal        => { sb_appendf(output, c!(" === ")); }
                    Binop::NotEqual     => { sb_appendf(output, c!(" !== ")); }
                    Binop::GreaterEqual => { sb_appendf(output, c!(" >= "));  }
                    Binop::LessEqual    => { sb_appendf(output, c!(" <= "));  }
                };
                generate_arg(rhs, output);
                sb_appendf(output, c!(");\n"));
            }
            Op::Funcall{result, name, args} => {
                sb_appendf(output, c!("memory.set_local(%zu, %s("), result - 1, name);
                for i in 0..args.count {
                    if i > 0 { sb_appendf(output, c!(", ")); }
                    generate_arg(*args.items.add(i), output);
                }
                sb_appendf(output, c!("));\n"));
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
    sb_appendf(output, c!("        memory.pop_stack();\n"));
    sb_appendf(output, c!("        return 0n;\n"));
    sb_appendf(output, c!("    }\n"));
    sb_appendf(output, c!("}\n"));
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func]) {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

// TODO: allow a runtime dynamic stack size somehow
// stack size in bytes, used for auto vars (32KiB -> 4096 auto vars)
const STACK_SIZE: usize = 32768;

pub unsafe fn generate_memory(output: *mut String_Builder, data: *const [u8], globals: *const [*const c_char]) {
    sb_appendf(output, c!("const memory = new Memory(\n    ["), data.len());

    for i in 0..data.len() {
        sb_appendf(output, c!("0x%02X,"), (*data)[i] as i64);
    }
    sb_appendf(output, c!("],\n    ["));

    for i in 0..globals.len() {
        sb_appendf(output, c!("\"%s\","), (*globals)[i] as i64);
    }
    sb_appendf(output, c!("],\n    %zu,\n);"), STACK_SIZE);
}

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    let template_cstr = c!(include_str!("html_js_template.tt"));
    let template_len = strlen(template_cstr);
    let generated = sv_from_cstr(c!("<<<GENERATED>>>"));
    let mut i = 0;
    while i < template_len {
        let prefix = sv_from_parts(template_cstr.add(i), template_len - i);
        if sv_starts_with(prefix, generated) {
            generate_memory(output, da_slice((*c).data), da_slice((*c).globals));
            generate_funcs(output, da_slice((*c).funcs));
            i += generated.count;
        } else {
            da_append(output, *template_cstr.add(i));
            i += 1;
        }
    }
}
