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

use core::ffi::*;
use core::cmp;
use core::mem::{zeroed, size_of};
use crust::libc::*;
use nob::*;
use targets::*;
use runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;

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
struct Stats {
    ks: usize,
    bs: usize,
    rs: usize,
}

#[derive(Clone, Copy)]
struct Job {
    child: Child_Process,
    case_index: usize,
    target_index: usize,
}

pub unsafe fn main(argc: i32, argv: *mut*mut c_char) -> Option<()> {
    let target_flags = flag_list(c!("t"), c!("Compilation targets to test on."));
    let cases_flags  = flag_list(c!("c"), c!("Test cases"));
    let test_folder  = flag_str(c!("dir"), c!("./tests/"), c!("Test folder"));
    let help         = flag_bool(c!("help"), false, c!("Print this help message"));
    let jobs_count   = flag_uint64(c!("j"), 4, c!("Amount of jobs to run in parallel."));
    // TODO: flag to print the list of tests
    // TODO: flag to print the list of targets
    // TODO: select test cases and targets by a glob pattern
    // See if https://github.com/tsoding/glob.h can be used here

    if !flag_parse(argc, argv) {
        usage();
        flag_print_error(stderr());
        return None;
    }

    if *help {
        usage();
        return Some(());
    }

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
        let mut test_files: File_Paths = zeroed();
        if !read_entire_dir(*test_folder, &mut test_files) { return None; }
        qsort(test_files.items as *mut c_void, test_files.count, size_of::<*const c_char>(), compar_cstr);

        for i in 0..test_files.count {
            let test_file = *test_files.items.add(i);
            if *test_file == '.' as c_char { continue; }
            let Some(case_name) = temp_strip_suffix(test_file, c!(".b")) else { continue; };
            da_append(&mut cases, case_name);
        }
    } else {
        for i in 0..(*cases_flags).count {
            let case_name = *(*cases_flags).items.add(i);
            da_append(&mut cases, case_name);
        }
    }

    if !mkdir_if_not_exists(GARBAGE_FOLDER) { return None; }

    let mut jobs: Array<Job> = zeroed();

    // pre-fill
    for i in 0..cases.count {
        let test_name = *cases.items.add(i);
        da_append(&mut reports, Report {
            name: test_name,
            statuses: zeroed(),
        });
        for _ in 0..targets.count {
            da_append(&mut (*reports.items.add(i)).statuses, Status::Ok);
        }
    }

    // build
    for i in 0..cases.count {
        let test_name = *cases.items.add(i);
        for j in 0..targets.count {
            let target = *targets.items.add(j);

            let mut cmd: Cmd = zeroed();
            let input_path = temp_sprintf(c!("%s/%s.b"), *test_folder, test_name);
            let output_path = temp_sprintf(c!("%s/%s.%s"), GARBAGE_FOLDER, test_name, match target {
                Target::Fasm_x86_64_Windows => c!("exe"),
                Target::Fasm_x86_64_Linux   => c!("fasm-x86_64-linux"),
                Target::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
                Target::Uxn                 => c!("rom"),
                Target::Mos6502             => c!("6502"),
            });
            cmd_append! {
                &mut cmd,
                c!("./build/b"),
                input_path,
                c!("-t"), name_of_target(target).unwrap(),
                c!("-o"), output_path,
            }

            let child = cmd_run_async(&mut cmd);
            if child == INVALID_PROC {
                *(*reports.items.add(i)).statuses.items.add(j) = Status::BuildFail;
                continue;
            };

            da_append(&mut jobs, Job { child, case_index: i, target_index: j });

            if jobs.count >= *jobs_count as usize {
                if let Some(job) = da_pop_first(&mut jobs) {
                    if !child_process_wait(job.child) {
                        *(*reports.items.add(job.case_index)).statuses.items.add(job.target_index) = Status::BuildFail;
                    }
                }
            }
        }
    }

    // wait for all jobs
    while jobs.count > 0 {
        if let Some(job) = da_pop_first(&mut jobs) {
            if !child_process_wait(job.child) {
                *(*reports.items.add(job.case_index)).statuses.items.add(job.target_index) = Status::BuildFail;
            }
        }
    }

    // run
    let mut sb: String_Builder = zeroed();
    for i in 0..cases.count {
        for j in 0..targets.count {
            if *(*reports.items.add(i)).statuses.items.add(j) as u32 != Status::Ok as u32 {
                continue;
            }

            let test_name = *cases.items.add(i);
            let target = *targets.items.add(j);
            let output_path = temp_sprintf(c!("%s/%s.%s"), GARBAGE_FOLDER, test_name, match target {
                Target::Fasm_x86_64_Windows => c!("exe"),
                Target::Fasm_x86_64_Linux   => c!("fasm-x86_64-linux"),
                Target::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
                Target::Uxn                 => c!("rom"),
                Target::Mos6502             => c!("6502"),
            });

            let mut cmd: Cmd = zeroed();
            let mut is_async = true;

            match target {
                Target::Fasm_x86_64_Linux | Target::Fasm_x86_64_Windows => {
                    cmd_append!(&mut cmd, output_path,);
                }
                Target::Gas_AArch64_Linux => {
                    cmd_append!(&mut cmd, c!("qemu-aarch64"), c!("-L"), c!("/usr/aarch64-linux-gnu"), output_path,);
                }
                Target::Uxn => {
                    cmd_append!(&mut cmd, c!("uxncli"), output_path,);
                }
                Target::Mos6502 => {
                    is_async = false;
                    let run_result = runner::mos6502::run(&mut sb, Config {
                        load_offset: DEFAULT_LOAD_OFFSET
                    }, output_path);
                    if run_result.is_none() {
                        *(*reports.items.add(i)).statuses.items.add(j) = Status::RunFail;
                    }
                }
            }

            if is_async {
                let child = cmd_run_async(&mut cmd);
                if child == INVALID_PROC {
                    *(*reports.items.add(i)).statuses.items.add(j) = Status::RunFail;
                } else {
                    da_append(&mut jobs, Job { child, case_index: i, target_index: j });
                }
            }

            if jobs.count >= *jobs_count as usize {
                if let Some(job) = da_pop_first(&mut jobs) {
                    if !child_process_wait(job.child) {
                        *(*reports.items.add(job.case_index)).statuses.items.add(job.target_index) = Status::RunFail;
                    }
                }
            }
        }
    }

    while jobs.count > 0 {
        if let Some(job) = da_pop_first(&mut jobs) {
            if !child_process_wait(job.child) {
                *(*reports.items.add(job.case_index)).statuses.items.add(job.target_index) = Status::RunFail;
            }
        }
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

    let mut width = 0;
    for i in 0..reports.count {
        let report = *reports.items.add(i);
        width = cmp::max(width, strlen(report.name));
    }

    printf(c!("%*s\x1b[32mK\x1b[0m - success\n"), width + 2, c!(""));
    printf(c!("%*s\x1b[33mB\x1b[0m - failed to build\n"), width + 2, c!(""));
    printf(c!("%*s\x1b[31mR\x1b[0m - runtime error\n"), width + 2, c!(""));
    printf(c!("\n"));

    let mut target_column_width = 0;
    for j in 0..targets.count {
        let target = *targets.items.add(j);
        let width = 2*(j + 1) + strlen(name_of_target(target).unwrap());
        if width > target_column_width {
            target_column_width = width;
        }
    }

    for j in 0..targets.count {
        let target = *targets.items.add(j);
        let stats = *stats_by_target.items.add(j);
        printf(c!("%*s"), width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("┌─%-*s"), target_column_width - 2*j, name_of_target(target).unwrap());
        printf(c!(" \x1b[32mK\x1b[0m: %-3zu \x1b[33mB\x1b[0m: %-3zu \x1b[31mR\x1b[0m: %-3zu\n"), stats.ks, stats.bs, stats.rs);
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
        let stats = *stats_by_target.items.add(j);
        printf(c!("%*s"), width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("└─%-*s"), target_column_width - 2*j, name_of_target(target).unwrap());
        printf(c!(" \x1b[32mK\x1b[0m: %-3zu \x1b[33mB\x1b[0m: %-3zu \x1b[31mR\x1b[0m: %-3zu\n"), stats.ks, stats.bs, stats.rs);
    }

    printf(c!("\n"));
    printf(c!("%*s\x1b[32mK\x1b[0m - success\n"), width + 2, c!(""));
    printf(c!("%*s\x1b[33mB\x1b[0m - failed to build\n"), width + 2, c!(""));
    printf(c!("%*s\x1b[31mR\x1b[0m - runtime error\n"), width + 2, c!(""));

    Some(())
}
