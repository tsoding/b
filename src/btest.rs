// TODO: Make btest test historical mode

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
pub mod glob;

use core::ffi::*;
use core::cmp;
use core::mem::{zeroed, size_of};
use crust::libc::*;
use nob::*;
use targets::*;
use runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;
use glob::*;

const GARBAGE_FOLDER: *const c_char = c!("./build/tests/");

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

pub unsafe fn run_test(cmd: *mut Cmd, output: *mut String_Builder, test_folder: *const c_char, name: *const c_char, target: Target, build_only: bool) -> Status {
    // TODO: add timeouts for running and building in case they go into infinite loop or something
    let input_path = temp_sprintf(c!("%s/%s.b"), test_folder, name);
    let output_path = temp_sprintf(c!("%s/%s.%s"), GARBAGE_FOLDER, name, match target {
        Target::Fasm_x86_64_Windows => c!("exe"),
        Target::Fasm_x86_64_Linux   => c!("fasm-x86_64-linux"),
        Target::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
        Target::Gas_x86_64_Linux    => c!("gas-x86_64-linux"),
        Target::Gas_x86_64_Windows  => c!("exe"),
        Target::Uxn                 => c!("rom"),
        Target::Mos6502             => c!("6502"),
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
    if !build_only {
        let run_result = match target {
            Target::Fasm_x86_64_Linux   => runner::fasm_x86_64_linux::run(cmd, output_path, &[]),
            Target::Fasm_x86_64_Windows => runner::fasm_x86_64_windows::run(cmd, output_path, &[]),
            Target::Gas_AArch64_Linux   => runner::gas_aarch64_linux::run(cmd, output_path, &[]),
            Target::Gas_x86_64_Windows  => runner::gas_x86_64_windows::run(cmd, output_path, &[]),
            Target::Gas_x86_64_Linux    => runner::gas_x86_64_linux::run(cmd, output_path, &[]),
            Target::Uxn                 => runner::uxn::run(cmd, c!("uxncli"), output_path, &[]),
            Target::Mos6502             => runner::mos6502::run(output, Config {
                load_offset: DEFAULT_LOAD_OFFSET
            }, output_path),
        };
        if let None = run_result {
            return Status::RunFail;
        }
    }
    Status::Ok
}

pub unsafe fn usage() {
    fprintf(stderr(), c!("Usage: %s [OPTIONS]\n"), flag_program_name());
    fprintf(stderr(), c!("OPTIONS:\n"));
    flag_print_options(stderr());
}

pub unsafe extern "C" fn compar_cstr(a: *const c_void, b: *const c_void) -> c_int {
    strcmp(*(a as *const *const c_char), *(b as *const *const c_char))
}

// TODO: Each field of Stats corresponds to an enum value of Status.
#[derive(Clone, Copy)]
pub struct Stats {
    pub ks: usize,
    pub bs: usize,
    pub rs: usize,
}

const K: *const c_char = c!("\x1b[32mK\x1b[0m");
const B: *const c_char = c!("\x1b[33mB\x1b[0m");
const R: *const c_char = c!("\x1b[31mR\x1b[0m");

pub unsafe fn print_legend(row_width: usize) {
    printf(c!("%*s%s - success\n"),         row_width + 2, c!(""), K);
    printf(c!("%*s%s - failed to build\n"), row_width + 2, c!(""), B);
    printf(c!("%*s%s - runtime error\n"),   row_width + 2, c!(""), R);
}

pub unsafe fn print_top_labels(targets: *const [Target], stats_by_target: *const [Stats], row_width: usize, col_width: usize) {
    assert!(targets.len() == stats_by_target.len());
    for j in 0..targets.len() {
        let target = (*targets)[j];
        let stats = (*stats_by_target)[j];
        printf(c!("%*s"), row_width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        // TODO: these fancy unicode characters don't work well on mingw32 build via wine
        printf(c!("┌─%-*s"), col_width - 2*j, name_of_target(target).unwrap());
        printf(c!(" %s: %-3zu %s: %-3zu %s: %-3zu\n"), K, stats.ks, B, stats.bs, R, stats.rs);
    }
}

pub unsafe fn print_bottom_labels(targets: *const [Target], stats_by_target: *const [Stats], row_width: usize, col_width: usize) {
    assert!(targets.len() == stats_by_target.len());
    for j in (0..targets.len()).rev() {
        let target = (*targets)[j];
        let stats = (*stats_by_target)[j];
        printf(c!("%*s"), row_width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("└─%-*s"), col_width - 2*j, name_of_target(target).unwrap());
        printf(c!(" %s: %-3zu %s: %-3zu %s: %-3zu\n"), K, stats.ks, B, stats.bs, R, stats.rs);
    }
}

pub unsafe fn matches_glob(pattern: *const c_char, text: *const c_char) -> Option<bool> {
    match glob_utf8(pattern, text) {
        Glob_Result::MATCHED   => Some(true),
        Glob_Result::UNMATCHED => Some(false),
        result => {
            let error = match result {
                Glob_Result::OOM_ERROR      => c!("out of memory"),
                Glob_Result::ENCODING_ERROR => c!("encoding error"),
                Glob_Result::SYNTAX_ERROR   => c!("syntax error"),
                Glob_Result::UNMATCHED | Glob_Result::MATCHED => unreachable!(),
            };
            fprintf(stderr(), c!("ERROR: while matching pattern `%s`: %s\n"), pattern, error);
            return None;
        },
    }
}

pub unsafe fn main(argc: i32, argv: *mut*mut c_char) -> Option<()> {
    let target_flags = flag_list(c!("t"), c!("Compilation targets to test on. Supports globbing."));
    let list_targets = flag_bool(c!("tlist"), false, c!("Print the list of compilation targets"));
    let cases_flags  = flag_list(c!("c"), c!("Test cases to run. Supports globbing."));
    let list_cases   = flag_bool(c!("clist"), false, c!("Print the list of test cases"));
    let test_folder  = flag_str(c!("dir"), c!("./tests/"), c!("Test folder"));
    let build_only   = flag_bool(c!("build-only"), false, c!("Only build the tests but don't run them"));
    let help         = flag_bool(c!("help"), false, c!("Print this help message"));

    if !flag_parse(argc, argv) {
        usage();
        flag_print_error(stderr());
        return None;
    }

    if *help {
        usage();
        return Some(());
    }

    let mut sb: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();
    let mut reports: Array<Report> = zeroed();

    if *list_targets {
        fprintf(stderr(), c!("Compilation targets:\n"));
        for j in 0..TARGET_NAMES.len() {
            fprintf(stderr(), c!("    %s\n"), (*TARGET_NAMES)[j].name);
        }
        return Some(());
    }

    let mut targets: Array<Target> = zeroed();
    if (*target_flags).count == 0 {
        for j in 0..TARGET_NAMES.len() {
            da_append(&mut targets, (*TARGET_NAMES)[j].target);
        }
    } else {
        for j in 0..(*target_flags).count {
            let saved_count = targets.count;
            let pattern = *(*target_flags).items.add(j);
            for j in 0..TARGET_NAMES.len() {
                let Target_Name { name, target } = (*TARGET_NAMES)[j];
                if matches_glob(pattern, name)? {
                    da_append(&mut targets, target);
                }
            }
            if targets.count == saved_count {
                fprintf(stderr(), c!("ERROR: unknown target `%s`\n"), pattern);
                return None;
            }
        }
    }

    let mut all_cases: Array<*const c_char> = zeroed();

    let mut test_files: File_Paths = zeroed();
    if !read_entire_dir(*test_folder, &mut test_files) { return None; }
    qsort(test_files.items as *mut c_void, test_files.count, size_of::<*const c_char>(), compar_cstr);

    for i in 0..test_files.count {
        let test_file = *test_files.items.add(i);
        if *test_file == '.' as c_char { continue; }
        let Some(case_name) = temp_strip_suffix(test_file, c!(".b")) else { continue; };
        da_append(&mut all_cases, case_name);
    }

    if *list_cases {
        fprintf(stderr(), c!("Test cases:\n"));
        for i in 0..all_cases.count {
            fprintf(stderr(), c!("    %s\n"), *all_cases.items.add(i));
        }
        return Some(());
    }

    let mut cases: Array<*const c_char> = zeroed();
    if (*cases_flags).count == 0 {
        cases = all_cases;
    } else {
        for i in 0..(*cases_flags).count {
            let saved_count = cases.count;
            let pattern = *(*cases_flags).items.add(i);
            for i in 0..all_cases.count {
                let case_name = *all_cases.items.add(i);
                if matches_glob(pattern, case_name)? {
                    da_append(&mut cases, case_name);
                }
            }
            if cases.count == saved_count {
                fprintf(stderr(), c!("ERROR: unknown test case `%s`\n"), pattern);
                return None;
            }
        }
    }

    if !mkdir_if_not_exists(GARBAGE_FOLDER) { return None; }

    // TODO: Parallelize the test runner.
    // Probably using `cmd_run_async_and_reset`.
    // Also don't forget to add the `-j` flag.
    for i in 0..cases.count {
        let test_name = *cases.items.add(i);
        let mut report = Report {
            name: test_name,
            statuses: zeroed(),
        };
        for j in 0..targets.count {
            let target = *targets.items.add(j);
            da_append(&mut report.statuses, run_test(&mut cmd, &mut sb, *test_folder, test_name, target, *build_only));
        }
        da_append(&mut reports, report);
    }

    // TODO: generate HTML reports and deploy them somewhere automatically

    let mut stats_by_target: Array<Stats> = zeroed();
    for j in 0..targets.count {
        let mut stats: Stats = zeroed();
        for i in 0..reports.count {
            let report = *reports.items.add(i);
            match *report.statuses.items.add(j) {
                Status::Ok        => stats.ks += 1,
                Status::BuildFail => stats.bs += 1,
                Status::RunFail   => stats.rs += 1,
            }
        }
        da_append(&mut stats_by_target, stats);
    }

    let mut row_width = 0;
    for i in 0..reports.count {
        let report = *reports.items.add(i);
        row_width = cmp::max(row_width, strlen(report.name));
    }

    let mut col_width = 0;
    for j in 0..targets.count {
        let target = *targets.items.add(j);
        let width = 2*(j + 1) + strlen(name_of_target(target).unwrap());
        col_width = cmp::max(col_width, width);
    }

    print_legend(row_width);
    printf(c!("\n"));
    print_top_labels(da_slice(targets), da_slice(stats_by_target), row_width, col_width);
    for i in 0..reports.count {
        let report = *reports.items.add(i);
        printf(c!("%*s:"), row_width, report.name);
        for j in 0..report.statuses.count {
            let status = *report.statuses.items.add(j);
            match status {
                Status::Ok        => printf(c!(" %s"), K),
                Status::BuildFail => printf(c!(" %s"), B),
                Status::RunFail   => printf(c!(" %s"), R),
            };
        }
        printf(c!("\n"));
    }
    print_bottom_labels(da_slice(targets), da_slice(stats_by_target), row_width, col_width);
    printf(c!("\n"));
    print_legend(row_width);

    Some(())
}
