use core::ffi::*;
use crate::{Op, Arg, Func, Compiler};
use crate::nob::*;


pub unsafe fn generate_arg(arg: Arg, output: *mut String_Builder) {
    // TODO: convert all autovars to BigInt
    match arg {
        Arg::Ref(index)          => sb_appendf(output, c!("Number((new DataView(memory)).getBigUint64(vars[%zu]))"), index - 1),
        Arg::AutoVar(index)      => sb_appendf(output, c!("vars[%zu]"), index - 1),
        Arg::Literal(value)      => sb_appendf(output, c!("%ld"), value),
        Arg::DataOffset(offset)  => sb_appendf(output, c!("%ld"), offset),
    };
}

pub unsafe fn generate_function(name: *const c_char, auto_vars_count: usize, body: *const [Op], output: *mut String_Builder) {
    sb_appendf(output, c!("function %s() {\n"), name);
    if auto_vars_count > 0 {
        sb_appendf(output, c!("    let vars = Array(%zu).fill(0);\n"), auto_vars_count);
    }
    sb_appendf(output, c!("    let pc = 0;\n"));
    sb_appendf(output, c!("    while (pc < %zu) {\n"), body.len());
    sb_appendf(output, c!("        switch(pc) {\n"));
    for i in 0..body.len() {
        sb_appendf(output, c!("            case %zu: "), i);
        match (*body)[i] {
            Op::Store {index, arg} => {
                sb_appendf(output, c!("(new DataView(memory)).setBigUint64(vars[%zu], BigInt("), index - 1);
                generate_arg(arg, output);
                sb_appendf(output, c!("));\n"));
            },
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
            Op::BitOr{index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" | "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::BitAnd{index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" & "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::BitShl{index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" << "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::BitShr{index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" >> "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            },
            Op::Add {index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" + "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Sub {index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" - "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Mul {index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" * "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Mod {index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" %% "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
            }
            Op::Less {index, lhs, rhs} => {
                sb_appendf(output, c!("vars[%zu] = "), index - 1);
                generate_arg(lhs, output);
                sb_appendf(output, c!(" < "));
                generate_arg(rhs, output);
                sb_appendf(output, c!(";\n"));
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
        generate_function((*funcs)[i].name, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
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

pub unsafe fn generate_program(output: *mut String_Builder, c: *const Compiler) {
    sb_appendf(output, c!(r#"<!DOCTYPE html>
<html>
  <head>
    <title>B Program</title>
  </head>
  <body>
    <h2>Console:</h2>
    <pre id="log"></pre>
    <script>
    // The compile B program
"#));
    generate_data_section(output, da_slice((*c).data));
    generate_funcs(output, da_slice((*c).funcs));
    sb_appendf(output, c!(r#"
      // The B runtime
      const log = document.getElementById("log");
      let logBuffer = "";
      const utf8decoder = new TextDecoder();
      function __flush() {
          log.innerText += logBuffer;
          logBuffer = "";
      }
      function __print_string(s) {
          for (let i = 0; i < s.length; ++i) {
              logBuffer += s[i];
              if (s[i] === '\n') __flush();
          }
      }
      function putchar(code) {
          __print_string(String.fromCharCode(code));
      }
      function strlen(ptr) {
          return (new Uint8Array(memory, ptr)).indexOf(0);
      }
      function printf(fmt, ...args) {
          const n = strlen(fmt);
          // TODO: print formatting is not fully implemented
          const bytes = memory.slice(fmt, fmt+n);
          const str = utf8decoder.decode(bytes);

          let index = 0;
          const output = str.replaceAll("%%d", () => args[index++]);
          __print_string(output);
      }
      function malloc(size) {
          const ptr = memory.byteLength;
          memory.resize(ptr+size);
          return ptr;
      }
      function memset(ptr, byte, size) {
          let view = new Uint8Array(memory, ptr, size);
          let bytes = Array(size).fill(byte);
          view.set(bytes);
      }
      main();
    </script>
  </body>
</html>"#));
}
