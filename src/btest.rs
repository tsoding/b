#![no_main]
#![no_std]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_macros)]

#[macro_use]
pub mod crust;
#[macro_use]
pub mod nob;
pub mod targets;
pub mod runner;
pub mod flag;

use core::ffi::*;
use core::cmp;
use core::mem::zeroed;
use crust::libc::*;
use nob::*;
use targets::*;
use runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;

const TEST_NAMES: *const [*const c_char] = &[
    c!("args11-extrn"),
    c!("args11"),
    c!("args6"),
    c!("asm_fasm_x86_64_linux"),
    c!("asm_func_fasm_x86_64_linux"),
    c!("call_stack_args"),
    c!("compare"),
    c!("deref_assign"),
    c!("divmod"),
    c!("e"),
    c!("forward-declare"),
    c!("globals"),
    c!("goto"),
    c!("hello"),
    c!("inc_dec"),
    c!("lexer"),
    c!("literals"),
    c!("minus_2"),
    c!("multiple-postfix"),
    c!("recursion"),
    c!("ref"),
    c!("return"),
    c!("rvalue_call"),
    c!("stack_alloc"),
    c!("switch"),
    c!("ternary-assign"),
    c!("ternary-side-effect"),
    c!("ternary"),
    c!("unary_priority"),
    c!("upper"),
    c!("vector"),
];

#[derive(Copy, Clone)]
pub enum Status {
    Ok,
    BuildFail,
    RunFail,
}

#[derive(Copy, Clone)]
struct Report {
    name: *const c_char,
    statuses: Array<Status>,
}

pub unsafe fn run_test(cmd: *mut Cmd, output: *mut String_Builder, name: *const c_char, target: Target) -> Status {
    let input_path = temp_sprintf(c!("./tests/%s.b"), name);
    let output_path = temp_sprintf(c!("./build/tests/%s%s"), name, match target {
        Target::Fasm_x86_64_Windows => c!(".exe"),
        Target::Fasm_x86_64_Linux   => c!(""),
        Target::Gas_AArch64_Linux   => c!(""),
        Target::Uxn                 => c!(".rom"),
        Target::Mos6502             => c!(".6502"),
    });
    cmd_append! {
        cmd,
        c!("./build/b"),
        input_path,
        c!("-t"), name_of_target(target).unwrap(),
        c!("-o"), output_path,
    }
    if !cmd_run_sync_and_reset(cmd) {
        return Status::BuildFail;
    }
    let run_result = match target {
        Target::Fasm_x86_64_Linux   => runner::fasm_x86_64_linux::run(cmd, output_path, &[]),
        Target::Fasm_x86_64_Windows => runner::fasm_x86_64_windows::run(cmd, output_path, &[]),
        Target::Gas_AArch64_Linux   => runner::gas_aarch64_linux::run(cmd, output_path, &[]),
        Target::Uxn                 => runner::uxn::run(cmd, c!("uxncli"), output_path, &[]),
        Target::Mos6502             => runner::mos6502::run(output, Config {
            load_offset: DEFAULT_LOAD_OFFSET
        }, output_path),
    };
    if let None = run_result {
        return Status::RunFail;
    }
    Status::Ok
}

pub unsafe fn usage() {
    fprintf(stderr(), c!("Usage: %s [OPTIONS]\n"), flag_program_name());
    fprintf(stderr(), c!("OPTIONS:\n"));
    flag_print_options(stderr());
}

pub unsafe fn main(argc: i32, argv: *mut*mut c_char) -> Option<()> {
    let target_flags = flag_list(c!("t"), c!("Compilation targets to test on."));
    let cases_flags = flag_list(c!("c"), c!("Test cases"));

    if !flag_parse(argc, argv) {
        usage();
        flag_print_error(stderr());
        return None;
    }

    let mut output: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();
    let mut reports: Array<Report> = zeroed();

    let mut targets: Array<Target> = zeroed();
    if (*target_flags).count == 0 {
        for j in 0..TARGET_NAMES.len() {
            let Target_Name { name: _, target } = (*TARGET_NAMES)[j];
            da_append(&mut targets, target);
        }
    } else {
        for j in 0..(*target_flags).count {
            let target_name = *(*target_flags).items.add(j);
            if let Some(target) = target_by_name(target_name) {
                da_append(&mut targets, target);
            } else {
                fprintf(stderr(), c!("ERRRO: unknown target `%s`\n"), target_name);
                return None;
            }
        }
    }

    let mut cases: Array<*const c_char> = zeroed();
    if (*cases_flags).count == 0 {
        for i in 0..TEST_NAMES.len() {
            da_append(&mut cases, (*TEST_NAMES)[i]);
        }
    } else {
        for i in 0..(*cases_flags).count {
            let case_name = *(*cases_flags).items.add(i);
            da_append(&mut cases, case_name);
        }
    }

    if !mkdir_if_not_exists(c!("./build/tests/")) { return None; }

    for i in 0..cases.count {
        let test_name = *cases.items.add(i);
        let mut report = Report {
            name: test_name,
            statuses: zeroed(),
        };
        for j in 0..targets.count {
            let target = *targets.items.add(j);
            da_append(&mut report.statuses, run_test(&mut cmd, &mut output, test_name, target));
            // da_append(&mut report.statuses, Status::Ok);
        }
        da_append(&mut reports, report);
    }

    let mut width = 0;
    for i in 0..reports.count {
        let report = *reports.items.add(i);
        width = cmp::max(width, strlen(report.name));
    }

    for j in 0..targets.count {
        let target = *targets.items.add(j);
        printf(c!("%*s"), width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("┌─%s\n"), name_of_target(target).unwrap(), j);
    }

    for i in 0..reports.count {
        let report = *reports.items.add(i);
        printf(c!("%*s:"), width, report.name);
        for j in 0..report.statuses.count {
            let status = *report.statuses.items.add(j);
            match status {
                Status::Ok        => printf(c!(" \x1b[32mK\x1b[0m")),
                Status::BuildFail => printf(c!(" \x1b[33mB\x1b[0m")),
                Status::RunFail   => printf(c!(" \x1b[31mR\x1b[0m")),
            };
        }
        printf(c!("\n"));
    }

    for j in (0..targets.count).rev() {
        let target = *targets.items.add(j);
        printf(c!("%*s"), width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("└─%s\n"), name_of_target(target).unwrap(), j);
    }

    Some(())
}
