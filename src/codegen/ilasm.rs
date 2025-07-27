use core::ffi::*;
use crate::nob::*;
use crate::crust::libc::*;
use crate::lexer::*;
use crate::missingf;
use crate::ir::*;
use core::mem::zeroed;

pub unsafe fn load_arg(loc: Loc, arg: Arg, output: *mut String_Builder, _data: *const [u8]) {
    match arg {
        Arg::Bogus           => unreachable!("bogus-amogus"),
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("        ldloc V_%zu\n"), index);
        }
        Arg::Deref(index)       => {
            sb_appendf(output, c!("        ldloc V_%zu\n"), index);
            sb_appendf(output, c!("        ldind.i8\n"));
        }
        Arg::RefAutoVar(..)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(name) => {
            sb_appendf(output, c!("        ldsflda int64 Program::%s\n"), name);
        }
        Arg::External(name)    => {
            sb_appendf(output, c!("        ldsfld int64 Program::%s\n"), name);
        }
        Arg::Literal(literal) => {
            sb_appendf(output, c!("        ldc.i8 %zd\n"), literal);
        }
        Arg::DataOffset(offset) => {
            sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
            sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
            sb_appendf(output, c!("        add\n"));
        },
    }
}

pub unsafe fn call_arg(loc: Loc, fun: Arg, out: *mut String_Builder, arity: usize) {
    match fun {
        Arg::Bogus           => unreachable!("bogus-amogus"),
        Arg::AutoVar(..)     => missingf!(loc, c!("AutoVar\n")),
        Arg::Deref(..)       => missingf!(loc, c!("Deref\n")),
        Arg::RefAutoVar(..)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(..) => missingf!(loc, c!("RefExternal\n")),
        Arg::External(name)  => {
            // TODO: unhardcode the printf
            //   The main difficulty here will be passing the string, since B ilasm-mono runtime only operates on int64
            //   as of right now. Some hack is required in here. Look into the direction of boxing the values.
            if strcmp(name, c!("printf")) == 0 {
                for _i in 0..arity - 1 {
                    sb_appendf(out, c!("        pop\n"));
                }
                sb_appendf(out, c!("        newobj instance void [mscorlib]System.String::.ctor(int8*)\n"));
                sb_appendf(out, c!("        call void class [mscorlib]System.Console::Write(string)\n"));
                sb_appendf(out, c!("        ldc.i8 0\n"));
            } else {
                sb_appendf(out, c!("        call int64 class Program::'%s'("), name); // If the function we want to call collides with a instruction
                                                                                      // we will get a syntax error so '' are necessary.
                for i in 0..arity {
                    if i > 0 { sb_appendf(out, c!(", ")); }
                    sb_appendf(out, c!("int64"));
                }
                sb_appendf(out, c!(")\n"));
            }
        },
        Arg::Literal(..)     => missingf!(loc, c!("Literal\n")),
        Arg::DataOffset(..)  => missingf!(loc, c!("DataOffset\n")),
    }
}

pub unsafe fn generate_function(func: Func, output: *mut String_Builder, data: *const [u8]) {
    sb_appendf(output, c!("    .method static int64 '%s' ("), func.name); // If the function we want to define collides with a instruction
                                                                          // we will get a syntax error so '' are necessary.
    for i in 0..func.params_count {
        if i > 0 { sb_appendf(output, c!(", ")); }
        sb_appendf(output, c!("int64"));
    }
    sb_appendf(output, c!(") {\n"), func.name);

    if !(func.body.count == 1 && matches!((*func.body.items.add(0)).opcode, Op::Asm {..})) {
        if func.auto_vars_count > 0 {
            sb_appendf(output, c!("        .locals init ("));
            for i in 0..func.auto_vars_count {
                if i > 0 { sb_appendf(output, c!(", ")); }
                sb_appendf(output, c!("int64 V_%zu"), i + 1);
            }
            sb_appendf(output, c!(")\n"));
        }

        sb_appendf(output, c!("        .maxstack %zu\n"), 16); // By default max execution stack size is 8, which isn't enough
                                                               // in cases where we need to pass more than 8 args to a function call.
                                                               // TODO: Find a way to unhardcode this?
        if func.params_count > 0 {
            for i in 0..func.params_count {
                sb_appendf(output, c!("        ldarg %zu\n"), i);
                sb_appendf(output, c!("        stloc V_%zu\n"), i + 1);
            }
        }
    }

    for i in 0..func.body.count {
        let op = *func.body.items.add(i);
        match op.opcode {
            Op::Bogus               => unreachable!("bogus-amogus"),
            Op::UnaryNot {result, arg}       => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        ldc.i8 0\n"));
                sb_appendf(output, c!("        ceq\n"));
                sb_appendf(output, c!("        conv.i8\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::Negate {result, arg}         => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        neg\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::Asm {stmts} => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    sb_appendf(output, c!("        %s\n"), stmt.line);
                }
            }
            Op::Binop {binop, index, lhs, rhs} => {
                load_arg(op.loc, lhs, output, data);
                load_arg(op.loc, rhs, output, data);
                match binop {
                    Binop::Plus         => sb_appendf(output, c!("        add\n")),
                    Binop::Minus        => sb_appendf(output, c!("        sub\n")),
                    Binop::Mult         => {
                        sb_appendf(output, c!("        mul\n"))
                    }
                    Binop::Div          => {
                        sb_appendf(output, c!("        div\n"))
                    }
                    Binop::Mod          => {
                        sb_appendf(output, c!("        rem\n"))
                    }
                    Binop::Equal        => {
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::NotEqual     => {
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        ldc.i4.0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::Less         => {
                        sb_appendf(output, c!("        clt\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    },
                    Binop::LessEqual    => {
                        sb_appendf(output, c!("        cgt\n"));
                        sb_appendf(output, c!("        ldc.i4.0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::Greater      => {
                        sb_appendf(output, c!("        cgt\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::GreaterEqual => {
                        sb_appendf(output, c!("        clt\n"));
                        sb_appendf(output, c!("        ldc.i4 0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::BitOr        => {
                        sb_appendf(output, c!("        or\n"))
                    }
                    Binop::BitAnd       => sb_appendf(output, c!("        and\n")),
                    Binop::BitShl       => {
                        sb_appendf(output, c!("        conv.i4\n")); //Shift amount must be int32 according to the CLI specification
                        sb_appendf(output, c!("        shl\n"))
                    }
                    Binop::BitShr       => {
                        sb_appendf(output, c!("        conv.i4\n")); //Shift amount must be int32 according to the CLI specification
                        sb_appendf(output, c!("        shr\n"))
                    }
                };
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::Index {result, arg, offset} => {
                load_arg(op.loc, arg, output, data);
                load_arg(op.loc, offset, output, data);
                sb_appendf(output, c!("        ldc.i4.8\n"));
                sb_appendf(output, c!("        mul\n"));
                sb_appendf(output, c!("        add\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::AutoAssign {index, arg}     => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::ExternalAssign {name, arg} => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stsfld int64 Program::%s\n"), name);
            }
            Op::Store {index, arg} => {
                sb_appendf(output, c!("        ldloc V_%zu\n"), index);
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stind.i8\n"));
            }
            Op::Funcall {result, fun, args} => {
                for i in 0..args.count {
                    load_arg(op.loc, *args.items.add(i), output, data);
                }
                call_arg(op.loc, fun, output, args.count);
                sb_appendf(output, c!("        stloc V_%zu\n"), result);

            }
            Op::Label {label} => {
                sb_appendf(output, c!("    L%zu:\n"), label);
            }
            Op::JmpLabel {label} => {
                sb_appendf(output, c!("        br L%zu\n"), label);
            }
            Op::JmpIfNotLabel {label, arg}  => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        brfalse L%zu\n"), label);
            }
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg(op.loc, arg, output, data);
                }
                else {
                    sb_appendf(output, c!("        ldc.i8 0\n"));
                }
                sb_appendf(output, c!("        ret\n"));
            }
        }
    }

    if func.body.count < 1 || !matches!((*func.body.items.add(func.body.count - 1)).opcode, Op::Asm {..} | Op::Return {..}) {
        sb_appendf(output, c!("        ldc.i8 0\n"));
        sb_appendf(output, c!("        ret\n"));
    }

    sb_appendf(output, c!("    }\n"));
}

pub unsafe fn generate_funcs(funcs: *const [Func], output: *mut String_Builder, data: *const [u8]) {
    for i in 0..funcs.len() {
        let func = (*funcs)[i];
        generate_function(func, output, data);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!(".class '<BLangDataSection>' extends [mscorlib]System.Object {\n"));
        sb_appendf(output, c!("    .class nested assembly sealed 'DataSection' extends [mscorlib]System.ValueType {\n"));
        sb_appendf(output, c!("        .pack 1\n"));
        sb_appendf(output, c!("        .size %zu\n"), data.len());
        sb_appendf(output, c!("    }\n"));
        sb_appendf(output, c!("    .field assembly static initonly valuetype '<BLangDataSection>'/'DataSection' 'Data' at DataArray\n"));
        sb_appendf(output, c!("    .data cil DataArray = bytearray ("));

        for i in 0..data.len() {
            if i > 0 { sb_appendf(output, c!(" ")); }
            sb_appendf(output, c!("%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!(")\n"));
        sb_appendf(output, c!("}\n"));
    }
}

pub unsafe fn generate_fields(output: *mut String_Builder, globals: *const [Global], extrns: *const [*const c_char], mono: bool) {
    let has_globals = globals.len() > 0;
    if has_globals {
        for i in 0..globals.len() {
            sb_appendf(output, c!("    .field public static int64 '%s'\n"), (*globals)[i].name);
        }
    }

    let mut has_rand = false;
    for i in 0..extrns.len() {
        let extrn = (*extrns)[i];
        if strcmp(extrn, c!("rand")) == 0 {
            has_rand = true;
            sb_appendf(output, c!("    .field public static class [mscorlib]System.Random '<Random>'\n"));
            break;
        }
    }

    if has_globals || has_rand {
        sb_appendf(output, c!("    .method static void .cctor() {\n"));

        if has_globals {
            for i in 0..globals.len() {
                let global = (*globals)[i];
                let is_array = global.values.count > 1;
                if is_array {
                    sb_appendf(output, c!("        ldc.i8 %zd\n"), global.values.count * 8);
                    sb_appendf(output, c!("        call int64 Program::malloc(int64)\n"));
                    sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name);
                }

                for j in 0..global.values.count {
                    match *global.values.items.add(j) {
                        ImmediateValue::Literal(lit) => {
                            if !is_array {
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), lit);
                                sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                            } else {
                                sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                                sb_appendf(output, c!("        add\n"));
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), lit);
                                sb_appendf(output, c!("        stind.i8\n"))
                            }
                        },
                        ImmediateValue::Name(name) => {
                            if !is_array {
                                sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), name);
                                sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                            } else {
                                sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                                sb_appendf(output, c!("        add\n"));
                                sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), name);
                                sb_appendf(output, c!("        stind.i8\n"))
                            }
                        }
                        ImmediateValue::DataOffset(offset) => {
                            if !is_array {
                                sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
                                sb_appendf(output, c!("        add\n"));
                                sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                            } else {
                                sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                                sb_appendf(output, c!("        add\n"));
                                sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
                                sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
                                sb_appendf(output, c!("        add\n"));
                                sb_appendf(output, c!("        stind.i8\n"))
                            }
                        },
                    };
                }
            }
        }

        if has_rand {
            if mono {
                sb_appendf(output, c!("        newobj instance void [mscorlib]System.Random::.ctor()\n"));
            }
            else {
                sb_appendf(output, c!("        call class [mscorlib]System.Random [System.Runtime]System.Random::get_Shared()\n"));
            }

            sb_appendf(output, c!("        stsfld class [mscorlib]System.Random Program::'<Random>'\n"));
        }

        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    }\n"));
    }
}

pub unsafe fn generate_program(
    // Inputs
    p: *const Program, program_path: *const c_char, garbage_base: *const c_char, _linker: *const [*const c_char], debug: bool,
    // Temporaries
    output: *mut String_Builder, cmd: *mut Cmd,
    // Mono
    mono: bool,
) -> Option<()> {
    if debug { todo!("Debug information for ilasm-mono") }

    sb_appendf(output, c!(".assembly 'Main' {}\n"));
    sb_appendf(output, c!(".assembly extern mscorlib {}\n"));
    if !mono {
        sb_appendf(output, c!(".assembly extern System.Runtime {}\n"));
        sb_appendf(output, c!(".assembly extern System.Runtime.InteropServices {}\n"));
    }
    sb_appendf(output, c!(".module Main.%s\n"), if mono { c!("exe") } else { c!("dll") });

    let sliced_data = da_slice((*p).data);
    generate_data_section(output, sliced_data);

    sb_appendf(output, c!(".class Program extends [mscorlib]System.Object {\n"));
    generate_fields(output, da_slice((*p).globals), da_slice((*p).extrns), mono);

    generate_funcs(da_slice((*p).funcs), output, sliced_data);
    sb_appendf(output, c!("    .method static void Main (string[] args) {\n"));
    sb_appendf(output, c!("        .entrypoint\n"));
    sb_appendf(output, c!("        call int64 class Program::main()\n"));
    sb_appendf(output, c!("        pop\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));
    sb_appendf(output, c!("}\n"));

    let output_asm_path = temp_sprintf(c!("%s.il"), garbage_base);
    write_entire_file(output_asm_path, (*output).items as *const c_void, (*output).count)?;
    log(Log_Level::INFO, c!("generated %s"), output_asm_path);

    if mono {
        cmd_append! {
            cmd,
            c!("ilasm"), output_asm_path, temp_sprintf(c!("-output:%s"), program_path)
        }
    }
    else {
        cmd_append! {
            cmd,
            c!("ilasm"), c!("-dll"), output_asm_path, temp_sprintf(c!("-output:%s"), program_path)
        }
    }

    if !cmd_run_sync_and_reset(cmd) { return None; }

    if !mono {
        let base_path;
        if let Some(path) = temp_strip_suffix(program_path, c!(".dll")) {
            base_path = path;
        } else {
            base_path = program_path;
        }

        let config_output_path = temp_sprintf(c!("%s.runtimeconfig.json"), base_path);
        let config = c!("
        {
            \"runtimeOptions\": {
                \"tfm\": \"net9.0\",
                \"framework\": {
                    \"name\": \"Microsoft.NETCore.App\",
                    \"version\": \"9.0.0\"
                }
            }
        }");

        write_entire_file(config_output_path, config as *const c_void, strlen(config))?;
    }

    Some(())
}

pub unsafe fn run_program(cmd: *mut Cmd, program_path: *const c_char, run_args: *const [*const c_char], stdout_path: Option<*const c_char>, mono: bool) -> Option<()> {
    if mono && !cfg!(target_os = "windows") {
        cmd_append! {
            cmd,
            c!("mono"),
        }
    }

    if !mono {
        cmd_append! {
            cmd,
            c!("dotnet"),
        }
    }

    cmd_append!{ cmd, program_path, }

    da_append_many(cmd, run_args);

    if let Some(stdout_path) = stdout_path {
        let mut fdout = fd_open_for_write(stdout_path);
        let mut redirect: Cmd_Redirect = zeroed();
        redirect.fdout = &mut fdout;
        if !cmd_run_sync_redirect_and_reset(cmd, redirect) { return None; }
    } else {
        if !cmd_run_sync_and_reset(cmd) { return None; }
    }
    Some(())
}

// TODO: make this codegen self-contained eventually.
//   That is generate .exe directly without the help of ilasm.
