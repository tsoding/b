use core::ffi::*;
use core::mem::zeroed;
use crate::ir::*;
use crate::nob::*;
use crate::crust::libc::*;
use crate::lexer::Loc;
use crate::missingf;
use crate::diagf;
use crate::arena;
use crate::codegen::*;
use crate::lexer;
use crate::lexer::{Token, loc};

// UXN memory map
// 0x0000 - 0x00ff - zero page
// 0x0100 - entry point. ROM file gets loaded here
// then funcs, globals, data
// everything else is stack growing down from 64k
// our calling convention:
// 0x00 - stack pointer
// 0x02 - base pointer
// 0x04 - 0xff - function arguments
// return addresses will be stored in uxn own return stack.
// so we can use JSI to jump to a function and then JMP2r to return

#[derive(Clone, Copy)]
pub struct Assembler {
    pub label_count: usize,
    pub named_labels: Array<NamedLabel>,
    pub data_section_label: usize,
    pub resolved_addresses: Array<u16>, // maps label index to its byte offset
    pub patches: Array<Patch>,
}

#[derive(Clone, Copy)]
pub struct NamedLabel {
    pub name: *const c_char,
    pub label: usize,
}

#[derive(Clone, Copy)]
pub struct Patch {
    pub kind: PatchKind,
    pub label: usize,
    pub addr: u16,
    pub offset: u16,
}

#[derive(Clone, Copy)]
pub enum PatchKind {
    UpperAbsolute,
    LowerAbsolute,
    UpperRelative,
    LowerRelative,
}

pub unsafe fn get_or_create_label_by_name(a: *mut Assembler, name: *const c_char) -> usize {
    for i in 0..(*a).named_labels.count {
        let named_label = (*a).named_labels.items.add(i);
        if strcmp((*named_label).name, name) == 0 {
            return (*named_label).label;
        }
    }
    let new_label = create_label(a);
    da_append(&mut (*a).named_labels, NamedLabel{name, label: new_label});
    new_label
}

pub unsafe fn create_label(a: *mut Assembler) -> usize {
    let label = (*a).label_count;
    (*a).label_count += 1;
    da_append(&mut (*a).resolved_addresses, 0);
    label
}

pub unsafe fn link_label(a: *mut Assembler, label: usize, addr: usize) {
    *(*a).resolved_addresses.items.add(label) = addr as u16;
}

pub unsafe fn apply_patches(output: *mut String_Builder, a: *mut Assembler) -> Option<()> {
    for i in 0..(*a).patches.count {
        let patch = *(*a).patches.items.add(i);
        let addr = *(*a).resolved_addresses.items.add(patch.label);
        if addr == 0 {
            for j in 0..(*a).named_labels.count {
                let named_label = *(*a).named_labels.items.add(j);
                if named_label.label == patch.label {
                    log(Log_Level::ERROR, c!("uxn: Label '%s' was never linked"), named_label.name);
                    return None;
                }
            }
            log(Log_Level::ERROR, c!("uxn: Label #%ld was never linked"), patch.label);
            return None;
        }
        let offset = patch.offset;
        let byte = match patch.kind {
            PatchKind::UpperAbsolute => ((addr + 0x100 + offset) >> 8) & 0xff,
            PatchKind::LowerAbsolute => (addr + 0x100 + offset) & 0xff,
            PatchKind::UpperRelative => ((addr + offset).wrapping_sub(patch.addr).wrapping_sub(2) >> 8) & 0xff,
            PatchKind::LowerRelative => ((addr + offset).wrapping_sub(patch.addr).wrapping_sub(1)) & 0xff,
        };
        *(*output).items.add(patch.addr as usize) = byte as c_char;
    }
    Some(())
}

const SP: u8 = 0;
const BP: u8 = 2;
const FIRST_ARG: u8 = 4;

pub unsafe fn generate_asm_funcs(output: *mut String_Builder, asm_funcs: *const [AsmFunc], assembler: *mut Assembler) -> Option<()> {
    for i in 0..asm_funcs.len() {
        let asm_func = (*asm_funcs)[i];
        link_label(assembler, get_or_create_label_by_name(assembler, asm_func.name), (*output).count);
        process_asm_statements(output, da_slice(asm_func.body), assembler)?;
    }
    Some(())
}

pub unsafe fn usage(params: *const [Param]) {
    fprintf(stderr(), c!("uxn codegen for the B compiler\n"));
    fprintf(stderr(), c!("OPTIONS:\n"));
    print_params_help(params);
}

enum_with_order! {
    #[derive(Clone, Copy)]
    enum Uxn_Runner in UXN_RUNNER_ORDER {
        Uxncli,
        Uxnemu,
    }
}

impl Uxn_Runner {
    fn name(self) -> *const c_char {
        match self {
            Uxn_Runner::Uxncli => c!("uxncli"),
            Uxn_Runner::Uxnemu => c!("uxnemu"),
        }
    }

    fn description(self) -> *const c_char {
        match self {
            Uxn_Runner::Uxncli => c!("CLI Emulator"),
            Uxn_Runner::Uxnemu => c!("GUI Emulator"),
        }
    }

    unsafe fn from_name(name: *const c_char) -> Option<Self> {
        for i in 0..UXN_RUNNER_ORDER.len() {
            let runner = (*UXN_RUNNER_ORDER)[i];
            if strcmp(runner.name(), name) == 0 {
                return Some(runner);
            }
        }
        None
    }
}

struct Uxn {
    runner: Uxn_Runner
}

pub unsafe fn new(a: *mut arena::Arena, args: *const [*const c_char]) -> Option<*mut c_void> {
    let gen = arena::alloc_type::<Uxn>(a);

    let mut help = false;
    let mut runner_name = zeroed();
    let params = &[
        Param {
            name:        c!("help"),
            description: c!("Print this help message"),
            value:       ParamValue::Flag { var: &mut help },
        },
        Param {
            name:        c!("runner"),
            description: c!("What runner to use for the Uxn roms"),
            value:       ParamValue::String { var: &mut runner_name, default: Uxn_Runner::Uxnemu.name() },
        },
    ];

    if let Err(message) = parse_args(params, args) {
        usage(params);
        log(Log_Level::ERROR, c!("%s"), message);
        return None;
    }

    if help {
        usage(params);
        return None;
    }

    if let Some(runner) = Uxn_Runner::from_name(runner_name) {
        (*gen).runner = runner;
    } else {
        usage(params);
        log(Log_Level::ERROR, c!("Invalid Uxn runner name `%s`!"), runner_name);
        log(Log_Level::ERROR, c!("Valid names:"));
        for i in 0..UXN_RUNNER_ORDER.len() {
            let runner = (*UXN_RUNNER_ORDER)[i];
            log(Log_Level::ERROR, c!("    %s - %s"), runner.name(), runner.description());
        }
        return None;
    }

    Some(gen as *mut c_void)
}

pub unsafe fn generate_program(
    // Inputs
    _gen: *mut c_void, program: *const Program, program_path: *const c_char, _garbage_base: *const c_char,
    _nostdlib: bool, debug: bool,
    // Temporaries
    output: *mut String_Builder, _cmd: *mut Cmd,
) -> Option<()> {
    if debug { todo!("Debug information for uxn") }

    let mut assembler: Assembler = zeroed();
    assembler.data_section_label = create_label(&mut assembler);
    // set the top of the stack
    write_lit2(output, 0xffff);
    write_lit_stz2(output, SP);
    // call main or _start, _start having a priority
    let mut main_proc = c!("main");
    for i in 0..(*program).funcs.count {
        let name = (*(*program).funcs.items.add(i)).name;
        if strcmp(name, c!("_start")) == 0 {
            main_proc = c!("_start");
            break;
        }
    }
    write_op(output, UxnOp::JSI);
    write_label_rel(output, get_or_create_label_by_name(&mut assembler, main_proc), &mut assembler, 0);
    // break out of the vector we were returned from
    // also put this as the return address for the next vector which might be called
    let vector_return_label = create_label(&mut assembler);
    link_label(&mut assembler, vector_return_label, (*output).count);
    write_op(output, UxnOp::LIT2r);
    write_label_abs(output, vector_return_label, &mut assembler, 0);
    write_op(output, UxnOp::BRK);

    generate_funcs(output, da_slice((*program).funcs), &mut assembler)?;
    generate_asm_funcs(output, da_slice((*program).asm_funcs), &mut assembler)?;
    generate_extrns(da_slice((*program).extrns), da_slice((*program).funcs), da_slice((*program).asm_funcs), da_slice((*program).globals))?;
    generate_data_section(output, da_slice((*program).data), &mut assembler);
    generate_globals(output, da_slice((*program).globals), &mut assembler);

    apply_patches(output, &mut assembler)?;

    write_entire_file(program_path, (*output).items as *const c_void, (*output).count)?;
    log(Log_Level::INFO, c!("generated %s"), program_path);

    Some(())
}

pub unsafe fn run_program(
    // Inputs
    gen: *mut c_void, program_path: *const c_char, run_args: *const [*const c_char],
    // Temporaries
    cmd: *mut Cmd,
) -> Option<()> {
    let gen = gen as *mut Uxn;
    cmd_append! {cmd, (*gen).runner.name(), program_path}
    da_append_many(cmd, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}

pub unsafe fn generate_funcs(output: *mut String_Builder, funcs: *const [Func], assembler: &mut Assembler) -> Option<()> {
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].name_loc, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output, assembler)?;
    }
    Some(())
}

pub unsafe fn generate_function(name: *const c_char, name_loc: Loc, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut String_Builder, assembler: *mut Assembler) -> Option<()> {
    link_label(assembler, get_or_create_label_by_name(assembler, name), (*output).count);

    const MAX_ARGS: usize = (256 - FIRST_ARG as usize) / 2;

    if params_count > MAX_ARGS {
        missingf!(name_loc, c!("Too many parameters in function definition. We support only %zu but %zu were provided\n"), MAX_ARGS, params_count);
    }

    // put BP on stack
    write_lit_ldz2(output, SP);
    write_lit2(output, 2);
    write_op(output, UxnOp::SUB2);
    write_lit_stz2(output, SP);
    write_lit_ldz2(output, BP);
    write_lit_ldz2(output, SP);
    write_op(output, UxnOp::STA2);

    // save SP to BP
    write_lit_ldz2(output, SP);
    write_lit_stz2(output, BP);

    // alloc autovars
    write_lit_ldz2(output, SP);
    write_lit2(output, (auto_vars_count * 2) as u16);
    write_op(output, UxnOp::SUB2);
    write_lit_stz2(output, SP);

    // copy params to local vars
    for i in 0..params_count {
        write_lit_ldz2(output, FIRST_ARG + (i as u8) * 2);
        store_auto(output, i + 1);
    }

    // prepare our labels for each IR label
    let mut labels: Array<usize> = zeroed();
    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Label {..} => {
                da_append(&mut labels, create_label(assembler));
            }
            _ => {}
        }
    }

    // emit code
    for i in 0..body.len() {
        let op = (*body)[i];
        match op.opcode {
            Op::Bogus => unreachable!("bogus-amogus"),
            Op::UnaryNot {result, arg} => {
                load_arg(arg, op.loc, output, assembler);
                // if arg == 0 then 1 else 0
                write_op(output, UxnOp::LIT2);
                write_short(output, 0);
                write_op(output, UxnOp::EQU2);
                // extend result to short
                write_op(output, UxnOp::LIT);
                write_byte(output, 0);
                write_op(output, UxnOp::SWP);
                // store
                store_auto(output, result);
            }
            Op::Negate {result, arg} => {
                write_lit2(output, 0);
                load_arg(arg, op.loc, output, assembler);
                write_op(output, UxnOp::SUB2);
                store_auto(output, result);
            }
            Op::Binop {binop: Binop::Plus, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::ADD2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Minus, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::SUB2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Mult, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::MUL2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Mod, index, lhs, rhs} => {
                // TODO: long enough to be an intrinsic
                const A: u8 = FIRST_ARG;
                const B: u8 = FIRST_ARG + 2;
                load_arg(lhs, op.loc, output, assembler);
                write_lit(output, A);
                write_op(output, UxnOp::STZ2k);
                write_op(output, UxnOp::POP);
                // extract sign A, stash it in the return stack
                write_op(output, UxnOp::DUP2);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::SFT2);
                write_op(output, UxnOp::STH2k);
                // get abs value of A
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2kr);
                write_op(output, UxnOp::ADD2);

                load_arg(rhs, op.loc, output, assembler);
                write_lit(output, B);
                write_op(output, UxnOp::STZ2k);
                write_op(output, UxnOp::POP);
                // extract sign B, stash it in the return stack
                write_op(output, UxnOp::DUP2);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::SFT2);
                write_op(output, UxnOp::STH2k);
                // get abs value of B
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2kr);
                write_op(output, UxnOp::ADD2);

                // do unsigned division
                write_op(output, UxnOp::DIV2);

                // write sign back in
                write_op(output, UxnOp::EOR2r);
                write_op(output, UxnOp::STH2kr);
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2r);
                write_op(output, UxnOp::ADD2);

                // calculate remainder
                write_lit_ldz2(output, A);
                write_lit_ldz2(output, B);
                write_op(output, UxnOp::ROT2);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::SUB2);

                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Div, index, lhs, rhs} => {
                // TODO: long enough to be an intrinsic
                load_arg(lhs, op.loc, output, assembler);
                // extract sign A, stash it in the return stack
                write_op(output, UxnOp::DUP2);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::SFT2);
                write_op(output, UxnOp::STH2k);
                // get abs value of A
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2kr);
                write_op(output, UxnOp::ADD2);

                load_arg(rhs, op.loc, output, assembler);
                // extract sign B, stash it in the return stack
                write_op(output, UxnOp::DUP2);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::SFT2);
                write_op(output, UxnOp::STH2k);
                // get abs value of B
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2kr);
                write_op(output, UxnOp::ADD2);

                // do unsigned division
                write_op(output, UxnOp::DIV2);

                // write sign back in
                write_op(output, UxnOp::EOR2r);
                write_op(output, UxnOp::STH2kr);
                write_lit2(output, 0xffff);
                write_op(output, UxnOp::MUL2);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::STH2r);
                write_op(output, UxnOp::ADD2);

                store_auto(output, index);
            }
            Op::Binop {binop: Binop::LessEqual, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                load_arg(rhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::GTH2);
                write_lit(output, 1);
                write_op(output, UxnOp::EOR);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Less, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                load_arg(rhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::LTH2);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Greater, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                load_arg(rhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::GTH2);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::Equal, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::EQU2);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::NotEqual, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::NEQ2);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::GreaterEqual, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                load_arg(rhs, op.loc, output, assembler);
                write_lit2(output, 0x8000);
                write_op(output, UxnOp::EOR2);
                write_op(output, UxnOp::LTH2);
                write_lit(output, 1);
                write_op(output, UxnOp::EOR);
                write_lit(output, 0);
                write_op(output, UxnOp::SWP);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::BitOr, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::ORA2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::BitAnd, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::AND2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::BitShl, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::NIP);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::AND);
                write_lit(output, 16);
                write_op(output, UxnOp::MUL);
                write_op(output, UxnOp::SFT2);
                store_auto(output, index);
            }
            Op::Binop {binop: Binop::BitShr, index, lhs, rhs} => {
                load_arg(lhs, op.loc, output, assembler);
                load_arg(rhs, op.loc, output, assembler);
                write_op(output, UxnOp::NIP);
                write_lit(output, 0x0f);
                write_op(output, UxnOp::AND);
                write_op(output, UxnOp::SFT2);
                store_auto(output, index);
            }
            Op::AutoAssign {index, arg} => {
                load_arg(arg, op.loc, output, assembler);
                store_auto(output, index);
            }
            Op::ExternalAssign {name, arg} => {
                load_arg(arg, op.loc, output, assembler);
                write_op(output, UxnOp::LIT2);
                write_label_abs(output, get_or_create_label_by_name(assembler, name), assembler, 0);
                write_op(output, UxnOp::STA2);
            }
            Op::Store {index, arg} => {
                load_arg(arg, op.loc, output, assembler);
                write_lit_ldz2(output, BP);
                write_lit2(output, (index * 2) as u16);
                write_op(output, UxnOp::SUB2);
                write_op(output, UxnOp::LDA2);
                write_op(output, UxnOp::STA2);
            }
            Op::Funcall {result, fun, args} => {
                if args.count > MAX_ARGS.into() {
                    missingf!(op.loc, c!("Too many function call arguments. We support only %d but %zu were provided\n"), MAX_ARGS, args.count);
                }
                for i in 0..args.count {
                    load_arg(*args.items.add(i), op.loc, output, assembler);
                    write_lit_stz2(output, FIRST_ARG + (i as u8) * 2)
                }

                call_arg(fun, op.loc, output, assembler);
                write_lit_ldz2(output, FIRST_ARG);
                store_auto(output, result);
            }
            Op::Asm {stmts} => {
                process_asm_statements(output, da_slice(stmts), assembler)?;
            }
            Op::Label {label} => {
                link_label(assembler, *labels.items.add(label), (*output).count);
            }
            Op::JmpLabel {label} => {
                write_op(output, UxnOp::JMI);
                write_label_rel(output, *labels.items.add(label), assembler, 0);
            }
            Op::JmpIfNotLabel {label, arg} => {
                load_arg(arg, op.loc, output, assembler);
                write_lit2(output, 0);
                write_op(output, UxnOp::EQU2);
                write_op(output, UxnOp::JCI);
                write_label_rel(output, *labels.items.add(label), assembler, 0);
            }
            Op::Return {arg} => {
                // Put return value in the FIRST_ARG
                if let Some(arg) = arg {
                    load_arg(arg, op.loc,  output, assembler);
                } else {
                    write_lit2(output, 0);
                }
                write_lit_stz2(output, FIRST_ARG);

                // restore SP from BP
                write_lit_ldz2(output, BP);
                write_lit_stz2(output, SP);

                // pop BP from stack
                write_lit_ldz2(output, SP);
                write_op(output, UxnOp::LDA2);
                write_lit_stz2(output, BP);
                write_lit_ldz2(output, SP);
                write_lit2(output, 2);
                write_op(output, UxnOp::ADD2);
                write_lit_stz2(output, SP);

                // return
                write_op(output, UxnOp::JMP2r);
            }
            Op::Index {result, arg, offset} => {
                load_arg(arg, op.loc, output, assembler);
                load_arg(offset, op.loc, output, assembler);
                write_lit(output, 0x10);
                write_op(output, UxnOp::SFT2);
                write_op(output, UxnOp::ADD2);
                store_auto(output, result);
            },
        }
    }

    free(labels.items);

    // return value is 0
    write_lit2(output, 0);
    write_lit_stz2(output, FIRST_ARG);

    // restore SP from BP
    write_lit_ldz2(output, BP);
    write_lit_stz2(output, SP);

    // pop BP from stack
    write_lit_ldz2(output, SP);
    write_op(output, UxnOp::LDA2);
    write_lit_stz2(output, BP);
    write_lit_ldz2(output, SP);
    write_lit2(output, 2);
    write_op(output, UxnOp::ADD2);
    write_lit_stz2(output, SP);

    // return
    write_op(output, UxnOp::JMP2r);

    Some(())
}

pub unsafe fn write_op(output: *mut String_Builder, op: UxnOp) {
    write_byte(output, op as u8);
}

pub unsafe fn write_byte(output: *mut String_Builder, byte: u8) {
    da_append(output, byte as c_char);
}

pub unsafe fn write_label_rel(output: *mut String_Builder, label: usize, a: *mut Assembler, offset: usize) {
    da_append(&mut (*a).patches, Patch{
        kind: PatchKind::UpperRelative,
        label: label,
        addr: (*output).count as u16,
        offset: offset as u16,
    });
    write_byte(output, 0xff);
    da_append(&mut (*a).patches, Patch{
        kind: PatchKind::LowerRelative,
        label: label,
        addr: (*output).count as u16,
        offset: offset as u16,
    });
    write_byte(output, 0xff);
}

pub unsafe fn write_label_abs(output: *mut String_Builder, label: usize, a: *mut Assembler, offset: usize) {
    da_append(&mut (*a).patches, Patch{
        kind: PatchKind::UpperAbsolute,
        label: label,
        addr: (*output).count as u16,
        offset: offset as u16,
    });
    write_byte(output, 0xff);
    da_append(&mut (*a).patches, Patch{
        kind: PatchKind::LowerAbsolute,
        label: label,
        addr: (*output).count as u16,
        offset: offset as u16,
    });
    write_byte(output, 0xff);
}

pub unsafe fn write_short(output: *mut String_Builder, short: u16) {
    write_byte(output, (short >> 8) as u8);
    write_byte(output, (short & 0xff) as u8);
}

pub unsafe fn write_lit(output: *mut String_Builder, lit: u8) {
    write_op(output, UxnOp::LIT);
    write_byte(output, lit);
}

pub unsafe fn write_lit2(output: *mut String_Builder, lit: u16) {
    write_op(output, UxnOp::LIT2);
    write_short(output, lit);
}

pub unsafe fn write_lit_ldz2(output: *mut String_Builder, zp: u8) {
    write_lit(output, zp);
    write_op(output, UxnOp::LDZ2);
}

pub unsafe fn write_lit_stz2(output: *mut String_Builder, zp: u8) {
    write_lit(output, zp);
    write_op(output, UxnOp::STZ2);
}

pub unsafe fn write_infinite_loop(output: *mut String_Builder) {
    write_op(output, UxnOp::JMI);
    write_short(output, 0xfffd);
}

pub unsafe fn call_arg(arg: Arg, loc: Loc, output: *mut String_Builder, assembler: *mut Assembler) {
    match arg {
        Arg::RefExternal(name) | Arg::External(name) => {
            write_op(output, UxnOp::JSI);
            write_label_rel(output, get_or_create_label_by_name(assembler, name), assembler, 0);
        }
        arg => {
            load_arg(arg, loc, output, assembler);
            write_op(output, UxnOp::JSR2);
        }
    };
}

pub unsafe fn load_arg(arg: Arg, loc: Loc, output: *mut String_Builder, assembler: *mut Assembler) {
    match arg {
        Arg::Deref(index) => {
            write_lit_ldz2(output, BP);
            write_lit2(output, (index * 2) as u16);
            write_op(output, UxnOp::SUB2);
            write_op(output, UxnOp::LDA2);
            write_op(output, UxnOp::LDA2);
        }
        Arg::External(name) => {
            let label = get_or_create_label_by_name(assembler, name);
            write_op(output, UxnOp::LIT2);
            write_label_abs(output, label, assembler, 0);
            write_op(output, UxnOp::LDA2);
        }
        Arg::AutoVar(index) => {
            write_lit_ldz2(output, BP);
            write_lit2(output, (index * 2) as u16);
            write_op(output, UxnOp::SUB2);
            write_op(output, UxnOp::LDA2);
        }
        Arg::Literal(value) => {
            if value >= 65536 {
                diagf!(loc, c!("WARNING: constant `%llu` out of range for 16 bits\n"), value);
            }
            write_lit2(output, value as u16);
        }
        Arg::DataOffset(offset) => {
            write_op(output, UxnOp::LIT2);
            write_label_abs(output, (*assembler).data_section_label, assembler, 0);
            write_lit2(output, offset as u16);
            write_op(output, UxnOp::ADD2);
        }
        Arg::RefAutoVar(index) => {
            write_lit_ldz2(output, BP);
            write_lit2(output, (index * 2) as u16);
            write_op(output, UxnOp::SUB2);
        }
        Arg::RefExternal(name) => {
            let label = get_or_create_label_by_name(assembler, name);
            write_op(output, UxnOp::LIT2);
            write_label_abs(output, label, assembler, 0);
        }
        Arg::Bogus => unreachable!("bogus-amogus"),
    }
}

pub unsafe fn store_auto(output: *mut String_Builder, index: usize) {
    write_lit_ldz2(output, BP);
    write_lit2(output, (index * 2) as u16);
    write_op(output, UxnOp::SUB2);
    write_op(output, UxnOp::STA2);
}

pub unsafe fn generate_extrns(extrns: *const [*const c_char], funcs: *const [Func], asm_funcs: *const[AsmFunc], globals: *const [Global]) -> Option<()> {
    'skip_function_or_global: for i in 0..extrns.len() {
        // assemble a few "stdlib" functions which can't be programmed in B
        let name = (*extrns)[i];
        for j in 0..funcs.len() {
            let func = (*funcs)[j];
            if strcmp(func.name, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        for j in 0..asm_funcs.len() {
            let func = (*asm_funcs)[j];
            if strcmp(func.name, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        for j in 0..globals.len() {
            let global = (*globals)[j].name;
            if strcmp(global, name) == 0 {
                continue 'skip_function_or_global
            }
        }
        log(Log_Level::ERROR, c!("uxn: Unknown extrn: `%s`, can not link"), name);
        return None;
    }
    Some(())
}

pub unsafe fn generate_globals(output: *mut String_Builder, globals: *const [Global], assembler: *mut Assembler) {
    for i in 0..globals.len() {
        let global = (*globals)[i];
        link_label(assembler, get_or_create_label_by_name(assembler, global.name), (*output).count);
        if global.is_vec {
            let label = create_label(assembler);
            write_label_abs(output, label, assembler, 0);
            link_label(assembler, label, (*output).count);
        }
        for j in 0..global.values.count {
            match *global.values.items.add(j) {
                ImmediateValue::Literal(lit) => {
                    write_short(output, lit as u16);
                }
                ImmediateValue::Name(name) => {
                    write_label_abs(output, get_or_create_label_by_name(assembler, name), assembler, 0);
                }
                ImmediateValue::DataOffset(offset) => {
                    write_label_abs(output, (*assembler).data_section_label, assembler, offset);
                }
            }
        }
        for _ in global.values.count..global.minimum_size {
            write_short(output, 0);
        }
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8], assembler: *mut Assembler) {
    link_label(assembler, (*assembler).data_section_label, (*output).count);
    for i in 0..data.len() {
        write_byte(output, (*data)[i]);
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq)]
pub enum UxnOp {
    BRK    = 0x00,
    INC    = 0x01,
    POP    = 0x02,
    NIP    = 0x03,
    SWP    = 0x04,
    ROT    = 0x05,
    DUP    = 0x06,
    OVR    = 0x07,
    EQU    = 0x08,
    NEQ    = 0x09,
    GTH    = 0x0a,
    LTH    = 0x0b,
    JMP    = 0x0c,
    JCN    = 0x0d,
    JSR    = 0x0e,
    STH    = 0x0f,
    LDZ    = 0x10,
    STZ    = 0x11,
    LDR    = 0x12,
    STR    = 0x13,
    LDA    = 0x14,
    STA    = 0x15,
    DEI    = 0x16,
    DEO    = 0x17,
    ADD    = 0x18,
    SUB    = 0x19,
    MUL    = 0x1a,
    DIV    = 0x1b,
    AND    = 0x1c,
    ORA    = 0x1d,
    EOR    = 0x1e,
    SFT    = 0x1f,
    JCI    = 0x20,
    INC2   = 0x21,
    POP2   = 0x22,
    NIP2   = 0x23,
    SWP2   = 0x24,
    ROT2   = 0x25,
    DUP2   = 0x26,
    OVR2   = 0x27,
    EQU2   = 0x28,
    NEQ2   = 0x29,
    GTH2   = 0x2a,
    LTH2   = 0x2b,
    JMP2   = 0x2c,
    JCN2   = 0x2d,
    JSR2   = 0x2e,
    STH2   = 0x2f,
    LDZ2   = 0x30,
    STZ2   = 0x31,
    LDR2   = 0x32,
    STR2   = 0x33,
    LDA2   = 0x34,
    STA2   = 0x35,
    DEI2   = 0x36,
    DEO2   = 0x37,
    ADD2   = 0x38,
    SUB2   = 0x39,
    MUL2   = 0x3a,
    DIV2   = 0x3b,
    AND2   = 0x3c,
    ORA2   = 0x3d,
    EOR2   = 0x3e,
    SFT2   = 0x3f,
    JMI    = 0x40,
    INCr   = 0x41,
    POPr   = 0x42,
    NIPr   = 0x43,
    SWPr   = 0x44,
    ROTr   = 0x45,
    DUPr   = 0x46,
    OVRr   = 0x47,
    EQUr   = 0x48,
    NEQr   = 0x49,
    GTHr   = 0x4a,
    LTHr   = 0x4b,
    JMPr   = 0x4c,
    JCNr   = 0x4d,
    JSRr   = 0x4e,
    STHr   = 0x4f,
    LDZr   = 0x50,
    STZr   = 0x51,
    LDRr   = 0x52,
    STRr   = 0x53,
    LDAr   = 0x54,
    STAr   = 0x55,
    DEIr   = 0x56,
    DEOr   = 0x57,
    ADDr   = 0x58,
    SUBr   = 0x59,
    MULr   = 0x5a,
    DIVr   = 0x5b,
    ANDr   = 0x5c,
    ORAr   = 0x5d,
    EORr   = 0x5e,
    SFTr   = 0x5f,
    JSI    = 0x60,
    INC2r  = 0x61,
    POP2r  = 0x62,
    NIP2r  = 0x63,
    SWP2r  = 0x64,
    ROT2r  = 0x65,
    DUP2r  = 0x66,
    OVR2r  = 0x67,
    EQU2r  = 0x68,
    NEQ2r  = 0x69,
    GTH2r  = 0x6a,
    LTH2r  = 0x6b,
    JMP2r  = 0x6c,
    JCN2r  = 0x6d,
    JSR2r  = 0x6e,
    STH2r  = 0x6f,
    LDZ2r  = 0x70,
    STZ2r  = 0x71,
    LDR2r  = 0x72,
    STR2r  = 0x73,
    LDA2r  = 0x74,
    STA2r  = 0x75,
    DEI2r  = 0x76,
    DEO2r  = 0x77,
    ADD2r  = 0x78,
    SUB2r  = 0x79,
    MUL2r  = 0x7a,
    DIV2r  = 0x7b,
    AND2r  = 0x7c,
    ORA2r  = 0x7d,
    EOR2r  = 0x7e,
    SFT2r  = 0x7f,
    LIT    = 0x80,
    INCk   = 0x81,
    POPk   = 0x82,
    NIPk   = 0x83,
    SWPk   = 0x84,
    ROTk   = 0x85,
    DUPk   = 0x86,
    OVRk   = 0x87,
    EQUk   = 0x88,
    NEQk   = 0x89,
    GTHk   = 0x8a,
    LTHk   = 0x8b,
    JMPk   = 0x8c,
    JCNk   = 0x8d,
    JSRk   = 0x8e,
    STHk   = 0x8f,
    LDZk   = 0x90,
    STZk   = 0x91,
    LDRk   = 0x92,
    STRk   = 0x93,
    LDAk   = 0x94,
    STAk   = 0x95,
    DEIk   = 0x96,
    DEOk   = 0x97,
    ADDk   = 0x98,
    SUBk   = 0x99,
    MULk   = 0x9a,
    DIVk   = 0x9b,
    ANDk   = 0x9c,
    ORAk   = 0x9d,
    EORk   = 0x9e,
    SFTk   = 0x9f,
    LIT2   = 0xa0,
    INC2k  = 0xa1,
    POP2k  = 0xa2,
    NIP2k  = 0xa3,
    SWP2k  = 0xa4,
    ROT2k  = 0xa5,
    DUP2k  = 0xa6,
    OVR2k  = 0xa7,
    EQU2k  = 0xa8,
    NEQ2k  = 0xa9,
    GTH2k  = 0xaa,
    LTH2k  = 0xab,
    JMP2k  = 0xac,
    JCN2k  = 0xad,
    JSR2k  = 0xae,
    STH2k  = 0xaf,
    LDZ2k  = 0xb0,
    STZ2k  = 0xb1,
    LDR2k  = 0xb2,
    STR2k  = 0xb3,
    LDA2k  = 0xb4,
    STA2k  = 0xb5,
    DEI2k  = 0xb6,
    DEO2k  = 0xb7,
    ADD2k  = 0xb8,
    SUB2k  = 0xb9,
    MUL2k  = 0xba,
    DIV2k  = 0xbb,
    AND2k  = 0xbc,
    ORA2k  = 0xbd,
    EOR2k  = 0xbe,
    SFT2k  = 0xbf,
    LITr   = 0xc0,
    INCkr  = 0xc1,
    POPkr  = 0xc2,
    NIPkr  = 0xc3,
    SWPkr  = 0xc4,
    ROTkr  = 0xc5,
    DUPkr  = 0xc6,
    OVRkr  = 0xc7,
    EQUkr  = 0xc8,
    NEQkr  = 0xc9,
    GTHkr  = 0xca,
    LTHkr  = 0xcb,
    JMPkr  = 0xcc,
    JCNkr  = 0xcd,
    JSRkr  = 0xce,
    STHkr  = 0xcf,
    LDZkr  = 0xd0,
    STZkr  = 0xd1,
    LDRkr  = 0xd2,
    STRkr  = 0xd3,
    LDAkr  = 0xd4,
    STAkr  = 0xd5,
    DEIkr  = 0xd6,
    DEOkr  = 0xd7,
    ADDkr  = 0xd8,
    SUBkr  = 0xd9,
    MULkr  = 0xda,
    DIVkr  = 0xdb,
    ANDkr  = 0xdc,
    ORAkr  = 0xdd,
    EORkr  = 0xde,
    SFTkr  = 0xdf,
    LIT2r  = 0xe0,
    INC2kr = 0xe1,
    POP2kr = 0xe2,
    NIP2kr = 0xe3,
    SWP2kr = 0xe4,
    ROT2kr = 0xe5,
    DUP2kr = 0xe6,
    OVR2kr = 0xe7,
    EQU2kr = 0xe8,
    NEQ2kr = 0xe9,
    GTH2kr = 0xea,
    LTH2kr = 0xeb,
    JMP2kr = 0xec,
    JCN2kr = 0xed,
    JSR2kr = 0xee,
    STH2kr = 0xef,
    LDZ2kr = 0xf0,
    STZ2kr = 0xf1,
    LDR2kr = 0xf2,
    STR2kr = 0xf3,
    LDA2kr = 0xf4,
    STA2kr = 0xf5,
    DEI2kr = 0xf6,
    DEO2kr = 0xf7,
    ADD2kr = 0xf8,
    SUB2kr = 0xf9,
    MUL2kr = 0xfa,
    DIV2kr = 0xfb,
    AND2kr = 0xfc,
    ORA2kr = 0xfd,
    EOR2kr = 0xfe,
    SFT2kr = 0xff,
}

pub const AsmOpNames: *const [*const c_char] = &[
    c!("brk"),    c!("inc"),    c!("pop"),    c!("nip"),    c!("swp"),    c!("rot"),    c!("dup"),    c!("ovr"),
    c!("equ"),    c!("neq"),    c!("gth"),    c!("lth"),    c!("jmp"),    c!("jcn"),    c!("jsr"),    c!("sth"),
    c!("ldz"),    c!("stz"),    c!("ldr"),    c!("str"),    c!("lda"),    c!("sta"),    c!("dei"),    c!("deo"),
    c!("add"),    c!("sub"),    c!("mul"),    c!("div"),    c!("and"),    c!("ora"),    c!("eor"),    c!("sft"),
    c!("jci"),    c!("inc2"),   c!("pop2"),   c!("nip2"),   c!("swp2"),   c!("rot2"),   c!("dup2"),   c!("ovr2"),
    c!("equ2"),   c!("neq2"),   c!("gth2"),   c!("lth2"),   c!("jmp2"),   c!("jcn2"),   c!("jsr2"),   c!("sth2"),
    c!("ldz2"),   c!("stz2"),   c!("ldr2"),   c!("str2"),   c!("lda2"),   c!("sta2"),   c!("dei2"),   c!("deo2"),
    c!("add2"),   c!("sub2"),   c!("mul2"),   c!("div2"),   c!("and2"),   c!("ora2"),   c!("eor2"),   c!("sft2"),
    c!("jmi"),    c!("incr"),   c!("popr"),   c!("nipr"),   c!("swpr"),   c!("rotr"),   c!("dupr"),   c!("ovrr"),
    c!("equr"),   c!("neqr"),   c!("gthr"),   c!("lthr"),   c!("jmpr"),   c!("jcnr"),   c!("jsrr"),   c!("sthr"),
    c!("ldzr"),   c!("stzr"),   c!("ldrr"),   c!("strr"),   c!("ldar"),   c!("star"),   c!("deir"),   c!("deor"),
    c!("addr"),   c!("subr"),   c!("mulr"),   c!("divr"),   c!("andr"),   c!("orar"),   c!("eorr"),   c!("sftr"),
    c!("jsi"),    c!("inc2r"),  c!("pop2r"),  c!("nip2r"),  c!("swp2r"),  c!("rot2r"),  c!("dup2r"),  c!("ovr2r"),
    c!("equ2r"),  c!("neq2r"),  c!("gth2r"),  c!("lth2r"),  c!("jmp2r"),  c!("jcn2r"),  c!("jsr2r"),  c!("sth2r"),
    c!("ldz2r"),  c!("stz2r"),  c!("ldr2r"),  c!("str2r"),  c!("lda2r"),  c!("sta2r"),  c!("dei2r"),  c!("deo2r"),
    c!("add2r"),  c!("sub2r"),  c!("mul2r"),  c!("div2r"),  c!("and2r"),  c!("ora2r"),  c!("eor2r"),  c!("sft2r"),
    c!("lit"),    c!("inck"),   c!("popk"),   c!("nipk"),   c!("swpk"),   c!("rotk"),   c!("dupk"),   c!("ovrk"),
    c!("equk"),   c!("neqk"),   c!("gthk"),   c!("lthk"),   c!("jmpk"),   c!("jcnk"),   c!("jsrk"),   c!("sthk"),
    c!("ldzk"),   c!("stzk"),   c!("ldrk"),   c!("strk"),   c!("ldak"),   c!("stak"),   c!("deik"),   c!("deok"),
    c!("addk"),   c!("subk"),   c!("mulk"),   c!("divk"),   c!("andk"),   c!("orak"),   c!("eork"),   c!("sftk"),
    c!("lit2"),   c!("inc2k"),  c!("pop2k"),  c!("nip2k"),  c!("swp2k"),  c!("rot2k"),  c!("dup2k"),  c!("ovr2k"),
    c!("equ2k"),  c!("neq2k"),  c!("gth2k"),  c!("lth2k"),  c!("jmp2k"),  c!("jcn2k"),  c!("jsr2k"),  c!("sth2k"),
    c!("ldz2k"),  c!("stz2k"),  c!("ldr2k"),  c!("str2k"),  c!("lda2k"),  c!("sta2k"),  c!("dei2k"),  c!("deo2k"),
    c!("add2k"),  c!("sub2k"),  c!("mul2k"),  c!("div2k"),  c!("and2k"),  c!("ora2k"),  c!("eor2k"),  c!("sft2k"),
    c!("litr"),   c!("inckr"),  c!("popkr"),  c!("nipkr"),  c!("swpkr"),  c!("rotkr"),  c!("dupkr"),  c!("ovrkr"),
    c!("equkr"),  c!("neqkr"),  c!("gthkr"),  c!("lthkr"),  c!("jmpkr"),  c!("jcnkr"),  c!("jsrkr"),  c!("sthkr"),
    c!("ldzkr"),  c!("stzkr"),  c!("ldrkr"),  c!("strkr"),  c!("ldakr"),  c!("stakr"),  c!("deikr"),  c!("deokr"),
    c!("addkr"),  c!("subkr"),  c!("mulkr"),  c!("divkr"),  c!("andkr"),  c!("orakr"),  c!("eorkr"),  c!("sftkr"),
    c!("lit2r"),  c!("inc2kr"), c!("pop2kr"), c!("nip2kr"), c!("swp2kr"), c!("rot2kr"), c!("dup2kr"), c!("ovr2kr"),
    c!("equ2kr"), c!("neq2kr"), c!("gth2kr"), c!("lth2kr"), c!("jmp2kr"), c!("jcn2kr"), c!("jsr2kr"), c!("sth2kr"),
    c!("ldz2kr"), c!("stz2kr"), c!("ldr2kr"), c!("str2kr"), c!("lda2kr"), c!("sta2kr"), c!("dei2kr"), c!("deo2kr"),
    c!("add2kr"), c!("sub2kr"), c!("mul2kr"), c!("div2kr"), c!("and2kr"), c!("ora2kr"), c!("eor2kr"), c!("sft2kr"),
];

pub unsafe fn find_opcode_by_name(name: *const c_char) -> Option<UxnOp> {
    let lname = strdup(name);
    for i in 0..strlen(name) {
        *(lname.add(i)) = tolower(*(name.add(i)) as c_int) as c_char;
    }
    for i in 0..AsmOpNames.len() {
        let op_name = (*AsmOpNames)[i];
        if strcmp(op_name, lname) == 0 {
            free(lname);
            return Some(core::mem::transmute(i as u8));
        }
    }
    free(lname);
    return None;
}

pub unsafe fn has_byte_immediate(op: UxnOp) -> bool {
    op == UxnOp::LIT
 || op == UxnOp::LITr
}

pub unsafe fn has_short_immediate(op: UxnOp) -> bool {
    op == UxnOp::JCI
 || op == UxnOp::JSI
 || op == UxnOp::JMI
 || op == UxnOp::LIT2
 || op == UxnOp::LIT2r
}

pub unsafe fn has_relative_immediate(op: UxnOp) -> bool {
    op == UxnOp::JCI
 || op == UxnOp::JSI
 || op == UxnOp::JMI
}

pub unsafe fn has_immediate(op: UxnOp) -> bool {
    has_byte_immediate(op) || has_short_immediate(op)
}

pub unsafe fn process_asm_statements(output: *mut String_Builder, asm_stmts: *const [AsmStmt], assembler: *mut Assembler) -> Option<()> {
    for i in 0..asm_stmts.len() {
        let asm_stmt = (*asm_stmts)[i];
        process_asm_statement(output, asm_stmt, assembler)?;
    }
    Some(())
}

pub unsafe fn process_asm_statement(output: *mut String_Builder, asm_stmt: AsmStmt, assembler: *mut Assembler) -> Option<()> {
    // TODO: leaky function, but beware holding onto strings produced by the lexer

    let mut lexer_name: String_Builder = zeroed();
    sb_appendf(&mut lexer_name, c!("%s:%d:%d <asm>"), asm_stmt.loc.input_path, asm_stmt.loc.line_number, asm_stmt.loc.line_offset);
    da_append(&mut lexer_name, 0);
    let mut l = lexer::new(lexer_name.items, asm_stmt.line, asm_stmt.line.add(strlen(asm_stmt.line)), false);
    let saved_point = l.parse_point;
    lexer::get_token(&mut l)?;
    match l.token {
        Token::EOF => { /* Allow empty asm line, not sure if useful */ }
        Token::ID => {
            // label or opcode
            lexer::get_token(&mut l)?;
            match l.token {
                Token::Colon => {
                    // label
                    link_label(assembler, get_or_create_label_by_name(assembler, l.string), (*output).count);
                    lexer::get_token(&mut l)?;
                    match l.token {
                        Token::ID => { /* must be an an opcode */ }
                        Token::EOF => { return Some(()); }
                        _ => {
                            diagf!(loc(&mut l), c!("ERROR: expected %s but got %s\n"),
                                lexer::display_token(Token::ID),
                                lexer::display_token(l.token));
                            return None;
                        }
                    }
                }
                _ => {
                    l.parse_point = saved_point;
                    lexer::get_token(&mut l)?;
                }
            }
        }
        _ => {
            diagf!(loc(&mut l), c!("ERROR: expected %s but got %s\n"),
                lexer::display_token(Token::ID), lexer::display_token(l.token));
            return None;
        }
    }
    // must be an opcode
    if let Some(opcode) = find_opcode_by_name(l.string) {
        write_op(output, opcode);
        lexer::get_token(&mut l)?;
        if has_immediate(opcode) {
            match l.token {
                Token::ID => {
                    // must be a label, only valid for short opcodes
                    if has_short_immediate(opcode) {
                        let label = get_or_create_label_by_name(assembler, l.string);
                        if has_relative_immediate(opcode) {
                            write_label_rel(output, label, assembler, 0);
                        } else {
                            write_label_abs(output, label, assembler, 0);
                        }
                    } else {
                        diagf!(loc(&mut l), c!("ERROR: label is not a valid short immediate\n"));
                        return None;
                    }
                }
                Token::IntLit | Token::CharLit => {
                    // immediate number literal
                    if has_short_immediate(opcode) {
                        write_short(output, l.int_number as u16);
                    } else {
                        write_byte(output, l.int_number as u8);
                    }
                }
                _ => {
                    diagf!(loc(&mut l), c!("ERROR: expected %s, %s, or %s but got %s\n"),
                        lexer::display_token(Token::ID),
                        lexer::display_token(Token::IntLit),
                        lexer::display_token(Token::CharLit),
                        lexer::display_token(l.token));
                    return None;
                }
            }
        } else {
            match l.token {
                Token::EOF => { return Some(()); }
                _ => {
                    diagf!(loc(&mut l), c!("ERROR: expected end of the line but got %s\n"),
                        lexer::display_token(l.token));
                    return None;
                }
            }
        }
    } else {
        diagf!(loc(&mut l), c!("ERROR: invalid uxn opcode: %s\n"), l.string);
        return None;
    }
    lexer::get_token(&mut l)?;
    match l.token {
        Token::EOF => { return Some(()); }
        _ => {
            diagf!(loc(&mut l), c!("ERROR: expected nothing but got %s\n"),
                lexer::display_token(l.token));
            return None;
        }
    }
}
