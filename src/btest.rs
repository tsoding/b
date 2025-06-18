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

use core::ffi::*;
use core::cmp;
use core::mem::zeroed;
use crust::libc::*;
use nob::*;
use targets::*;

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
enum Status {
    Ok,
    BuildFail,
    RunFail,
}

#[derive(Copy, Clone)]
struct Report {
    name: *const c_char,
    statuses: Array<Status>,
}

pub unsafe fn run_test(cmd: *mut Cmd, name: *const c_char, target: Target) -> Status {
    let input_path = temp_sprintf(c!("./tests/%s.b"), name);
    let output_path = temp_sprintf(c!("./build/tests/%s"), name);
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
    Status::Ok
}

pub unsafe fn main(mut _argc: i32, mut _argv: *mut*mut c_char) -> Option<()> {
    let mut cmd: Cmd = zeroed();
    let mut reports: Array<Report> = zeroed();
    for i in 0..TEST_NAMES.len() {
        let test_name = (*TEST_NAMES)[i];
        let mut report = Report {
            name: test_name,
            statuses: zeroed(),
        };
        for j in 0..TARGET_NAMES.len() {
            let Target_Name { name: _, target } = (*TARGET_NAMES)[j];
            da_append(&mut report.statuses, run_test(&mut cmd, test_name, target));
        }
        da_append(&mut reports, report);
    }

    let mut width = 0;
    for i in 0..reports.count {
        let report = *reports.items.add(i);
        width = cmp::max(width, strlen(report.name));
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

    Some(())
}
