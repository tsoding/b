use core::ffi::*;
use crate::nob::*;
use crate::crust::libc::*;
use crate::lexer::*;
use crate::missingf;
use crate::ir::*;
use crate::codegen::*;
use crate::arena;

pub unsafe fn load_arg(loc: Loc, arg: Arg, output: *mut String_Builder, data: *const [u8]) {
    match arg {
        Arg::Bogus           => unreachable!("bogus-amogus"),
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("        ldloc V_%zu\n"), index);
        }
        Arg::Deref(..)       => missingf!(loc, c!("Deref\n")),
        Arg::RefAutoVar(..)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(..) => missingf!(loc, c!("RefExternal\n")),
        Arg::External(..)    => missingf!(loc, c!("External\n")),
        Arg::Literal(literal) => {
            sb_appendf(output, c!("        ldc.i8 %zu\n"), literal);
        }
        Arg::DataOffset(offset) => {
            let mut p = offset;
            sb_appendf(output, c!("        ldstr \""), p);
            while (*data)[p] != 0 {
                let x = (*data)[p] as i32;
                if isprint(x) != 0 {
                    sb_appendf(output, c!("%c"), x);
                } else {
                    match x {
                        x if x == '\n' as i32 => { sb_appendf(output, c!("\\n")); }
                        _    => todo!(),
                    }
                }
                p += 1;
            }
            sb_appendf(output, c!("\"\n"));
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
                sb_appendf(out, c!("        call void class [mscorlib]System.Console::Write(string)\n"));
                sb_appendf(out, c!("        ldc.i8 0\n"));
            } else {
                sb_appendf(out, c!("        call int64 class Program::%s("), name);
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
    sb_appendf(output, c!("    .method static int64 '%s' ("), func.name);
    for i in 0..func.params_count {
        if i > 0 { sb_appendf(output, c!(", ")); }
        sb_appendf(output, c!("int64"));
    }
    sb_appendf(output, c!(") {\n"), func.name);

    if func.auto_vars_count > 0 {
        sb_appendf(output, c!("        .locals init ("));
        for i in 0..func.auto_vars_count {
            if i > 0 { sb_appendf(output, c!(", ")); }
            sb_appendf(output, c!("int64 V_%zu"), i + 1);
        }
        sb_appendf(output, c!(")\n"));
    }

    for i in 0..func.body.count {
        let op = *func.body.items.add(i);
        match op.opcode {
            Op::Bogus               => unreachable!("bogus-amogus"),
            Op::UnaryNot {..}       => missingf!(op.loc, c!("Op::UnaryNot\n")),
            Op::Negate {..}         => missingf!(op.loc, c!("Op::Negate\n")),
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
                    Binop::Minus        => missingf!(op.loc, c!("Binop::Minus\n")),
                    Binop::Mult         => missingf!(op.loc, c!("Binop::Mult\n")),
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
                    Binop::NotEqual     => missingf!(op.loc, c!("Binop::NotEqual\n")),
                    Binop::Less         => {
                        sb_appendf(output, c!("        clt\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    },
                    Binop::LessEqual    => {
                        sb_appendf(output, c!("        cgt\n"));
                        sb_appendf(output, c!("        ldc.i4 0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::Greater      => missingf!(op.loc, c!("Binop::Greater\n")),
                    Binop::GreaterEqual => missingf!(op.loc, c!("Binop::GreaterEqual\n")),
                    Binop::BitOr        => {
                        sb_appendf(output, c!("        or\n"))
                    }
                    Binop::BitAnd       => missingf!(op.loc, c!("Binop::BitAnd\n")),
                    Binop::BitShl       => missingf!(op.loc, c!("Binop::BitShl\n")),
                    Binop::BitShr       => missingf!(op.loc, c!("Binop::BitShr\n")),
                };
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::Index {..}          => missingf!(op.loc, c!("Op::Index\n")),
            Op::AutoAssign {index, arg}     => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::ExternalAssign {..} => missingf!(op.loc, c!("Op::ExternalAssign\n")),
            Op::Store {..}          => missingf!(op.loc, c!("Op::Store\n")),
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
            Op::Return {..}         => missingf!(op.loc, c!("Op::Return\n")),
        }
    }
    sb_appendf(output, c!("        ldc.i8 0\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));
}

pub unsafe fn generate_funcs(funcs: *const [Func], output: *mut String_Builder, data: *const [u8]) {
    for i in 0..funcs.len() {
        let func = (*funcs)[i];
        generate_function(func, output, data);
    }
}

pub unsafe fn usage(params: *const [Param]) {
    fprintf(stderr(), c!("ilasm_mono codegen for the B compiler\n"));
    fprintf(stderr(), c!("OPTIONS:\n"));
    print_params_help(params);
}

struct ILasm_Mono {
    output: String_Builder,
    cmd: Cmd,
}

pub unsafe fn new(a: *mut arena::Arena, args: *const [*const c_char]) -> Option<*mut c_void> {
    let gen = arena::alloc_type::<ILasm_Mono>(a);
    memset(gen as _ , 0, size_of::<ILasm_Mono>());

    let mut help = false;
    let params = &[
        Param {
            name:        c!("help"),
            description: c!("Print this help message"),
            value:       ParamValue::Flag { var: &mut help },
        },
    ];

    if let Err(message) = parse_args(params, args) {
        usage(params);
        log(Log_Level::ERROR, c!("%s"), message);
        return None;
    }

    if help {
        usage(params);
        fprintf(stderr(), c!("\n"));
        fprintf(stderr(), c!("It doesn't really provide any useful parameters yet.\n"));
        return None;
    }
    Some(gen as _)
}

pub unsafe fn generate_program(
    gen: *mut c_void, program: *const Program, program_path: *const c_char, garbage_base: *const c_char,
    _nostdlib: bool, debug: bool,
) -> Option<()> {
    let gen = gen as *mut ILasm_Mono;
    let output = &mut (*gen).output;
    let cmd = &mut (*gen).cmd;

    if debug { todo!("Debug information for ilasm-mono") }

    sb_appendf(output, c!(".assembly 'Main' {}\n"));
    sb_appendf(output, c!(".module Main.exe\n"));
    sb_appendf(output, c!(".class Program extends [mscorlib]System.Object {\n"));
    generate_funcs(da_slice((*program).funcs), output, da_slice((*program).data));
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

    cmd_append!{
        cmd,
        c!("ilasm"), output_asm_path, temp_sprintf(c!("/output:%s"), program_path)
    }

    if !cmd_run_sync_and_reset(cmd) { return None; }

    Some(())
}

pub unsafe fn run_program(
    gen: *mut c_void, program_path: *const c_char, run_args: *const [*const c_char],
) -> Option<()> {
    let gen = gen as *mut ILasm_Mono;
    let cmd = &mut (*gen).cmd;

    cmd_append!{
        cmd,
        c!("mono"), program_path,
    }

    da_append_many(cmd, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}

// TODO: make this codegen self-contained eventually.
//   That is generate .exe directly without the help of ilasm.
