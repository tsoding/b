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
pub mod jim;
pub mod jimp;

use core::ffi::*;
use core::cmp;
use core::mem::{zeroed, size_of};
use crust::slice_lookup;
use crust::libc::*;
use nob::*;
use targets::*;
use runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;
use jim::*;
use jimp::*;

const GARBAGE_FOLDER: *const c_char = c!("./build/tests/");

#[derive(Copy, Clone, PartialEq)]
pub enum OutcomeStatus {
    /// The test didn't even manage to build
    BuildFail,
    /// The test built and maybe even printed something, but crashed at the end
    RunFail,
    /// The test built, printed something and exited normally
    RunSuccess,
}

pub unsafe fn outcome_status_serialize(jim: *mut Jim, status: OutcomeStatus) {
    match status {
        OutcomeStatus::BuildFail  => jim_string(jim, c!("BuildFail")),
        OutcomeStatus::RunFail    => jim_string(jim, c!("RunFail")),
        OutcomeStatus::RunSuccess => jim_string(jim, c!("RunSuccess")),
    }
}

pub unsafe fn outcome_status_deserialize(jimp: *mut Jimp) -> Option<OutcomeStatus> {
    jimp_string(jimp)?;
    if strcmp((*jimp).string, c!("RunSuccess")) == 0 {
        Some(OutcomeStatus::RunSuccess)
    } else if strcmp((*jimp).string, c!("BuildFail")) == 0 {
        Some(OutcomeStatus::BuildFail)
    } else if strcmp((*jimp).string, c!("RunFail")) == 0 {
        Some(OutcomeStatus::RunFail)
    } else {
        // TODO: unknown member is not an accurate diagnostic here, but it works accidentally 'cause it reports on jimp->string
        jimp_unknown_member(jimp);
        None
    }
}

#[derive(Copy, Clone)]
pub struct Outcome {
    pub status: OutcomeStatus,
    pub stdout: *const c_char,
}

pub unsafe fn outcome_serialize(jim: *mut Jim, outcome: Outcome) {
    jim_object_begin(jim);
        jim_member_key(jim, c!("status"));
        outcome_status_serialize(jim, outcome.status);
        jim_member_key(jim, c!("stdout"));
        jim_string(jim, outcome.stdout);
    jim_object_end(jim);
}

pub unsafe fn outcome_deserialize(jimp: *mut Jimp) -> Option<Outcome> {
    let mut outcome: Outcome = zeroed();
    jimp_object_begin(jimp)?;
    while let Some(()) = jimp_object_member(jimp) {
        if strcmp((*jimp).string, c!("status")) == 0 {
            outcome.status = outcome_status_deserialize(jimp)?;
        } else if strcmp((*jimp).string, c!("stdout")) == 0 {
            jimp_string(jimp)?;
            outcome.stdout = strdup((*jimp).string); // TODO: memory leak
        } else {
            jimp_unknown_member(jimp);
            return None;
        }
    }
    jimp_object_end(jimp)?;
    Some(outcome)
}

#[derive(Copy, Clone)]
pub struct Report {
    pub name: *const c_char,
    /// The bool indicates whether the outcome was expected or not
    pub statuses: Array<(OutcomeStatus, bool)>,
}

pub unsafe fn execute_test(
    // Inputs
    test_folder: *const c_char, name: *const c_char, target: Target,
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder,
) -> Option<Outcome> {
    // TODO: add timeouts for running and building in case they go into infinite loop or something
    let input_path = temp_sprintf(c!("%s/%s.b"), test_folder, name);
    let program_path = temp_sprintf(c!("%s/%s.%s"), GARBAGE_FOLDER, name, match target {
        Target::Fasm_x86_64_Windows => c!("exe"),
        Target::Fasm_x86_64_Linux   => c!("fasm-x86_64-linux"),
        Target::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
        Target::Gas_x86_64_Linux    => c!("gas-x86_64-linux"),
        Target::Gas_x86_64_Windows  => c!("exe"),
        Target::Uxn                 => c!("rom"),
        Target::Mos6502             => c!("6502"),
    });
    let stdout_path = temp_sprintf(c!("%s/%s.%s.stdout.txt"), GARBAGE_FOLDER, name, name_of_target(target).unwrap());
    cmd_append! {
        cmd,
        c!("./build/b"),
        input_path,
        c!("-t"), name_of_target(target).unwrap(),
        c!("-o"), program_path,
    }
    if !cmd_run_sync_and_reset(cmd) {
        return Some(Outcome {
            status: OutcomeStatus::BuildFail,
            stdout: c!("")
        });
    }
    let run_result = match target {
        Target::Fasm_x86_64_Linux   => runner::fasm_x86_64_linux::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Fasm_x86_64_Windows => runner::fasm_x86_64_windows::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_AArch64_Linux   => runner::gas_aarch64_linux::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_x86_64_Linux    => runner::gas_x86_64_linux::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_x86_64_Windows  => runner::gas_x86_64_windows::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Uxn                 => runner::uxn::run(cmd, c!("uxncli"), program_path, &[], Some(stdout_path)),
        Target::Mos6502             => runner::mos6502::run(sb, Config {
            load_offset: DEFAULT_LOAD_OFFSET
        }, program_path, Some(stdout_path)),
    };

    (*sb).count = 0;
    read_entire_file(stdout_path, sb)?; // Should always succeed, but may fail if stdout_path is a directory for instance.
    da_append(sb, 0);                   // NULL-terminating the stdout
    printf(c!("%s"), (*sb).items);      // Forward stdout for diagnostic purposes

    Some(Outcome {
        status: if let None = run_result {
            OutcomeStatus::RunFail
        } else {
            OutcomeStatus::RunSuccess
        },
        stdout: strdup((*sb).items), // TODO: Memory leak
    })
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
    pub expected_ks:   usize,
    pub unexpected_ks: usize,
    pub expected_bs:   usize,
    pub unexpected_bs: usize,
    pub expected_rs:   usize,
    pub unexpected_rs: usize,
}

const RESET:  *const c_char = c!("\x1b[0m");
const GREEN:  *const c_char = c!("\x1b[32m");
const YELLOW: *const c_char = c!("\x1b[33m");
const GREY:   *const c_char = c!("\x1b[90m");
const RED:    *const c_char = c!("\x1b[31m");

const K_EXPECTED:   *const c_char = GREEN;
const K_UNEXPECTED: *const c_char = YELLOW;
const B_EXPECTED:   *const c_char = GREY;
const B_UNEXPECTED: *const c_char = RED;
const R_EXPECTED:   *const c_char = GREY;
const R_UNEXPECTED: *const c_char = RED;

pub unsafe fn print_legend(row_width: usize) {
    printf(c!("%*s%sK%s - success                 %sK%s - unexpected output\n"), row_width + 2, c!(""), K_EXPECTED, RESET, K_UNEXPECTED, RESET);
    printf(c!("%*s%sB%s - expected build error    %sB%s - build error\n"),       row_width + 2, c!(""), B_EXPECTED, RESET, B_UNEXPECTED, RESET);
    printf(c!("%*s%sR%s - expected runtime error  %sR%s - runtime error\n"),     row_width + 2, c!(""), R_EXPECTED, RESET, R_UNEXPECTED, RESET);
}

pub unsafe fn print_stats(stats: Stats) {
    printf(c!(" K: %s%3zu%s/%s%-3zu%s B: %s%3zu%s/%s%-3zu%s R: %s%3zu%s/%s%-3zu%s\n"),
           K_EXPECTED, stats.expected_ks, RESET, K_UNEXPECTED, stats.unexpected_ks, RESET,
           B_EXPECTED, stats.expected_bs, RESET, B_UNEXPECTED, stats.unexpected_bs, RESET,
           R_EXPECTED, stats.expected_rs, RESET, R_UNEXPECTED, stats.unexpected_rs, RESET);
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
        print_stats(stats)
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
        print_stats(stats)
    }
}

pub unsafe fn record_tests(
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target],
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder, jim: *mut Jim,
) -> Option<()> {
    // TODO: Parallelize the test runner.
    // Probably using `cmd_run_async_and_reset`.
    // Also don't forget to add the `-j` flag.
    for i in 0..cases.len() {
        let case_name = (*cases)[i];
        (*jim).sink_count = 0;
        (*jim).scopes_count = 0;
        jim_object_begin(jim);
        for j in 0..targets.len() {
            let target = (*targets)[j];
            jim_member_key(jim, name_of_target(target).unwrap());
            let outcome = execute_test(
                // Inputs
                test_folder, case_name, target,
                // Outputs
                cmd, sb,
            )?;
            outcome_serialize(jim, outcome);
        }
        jim_object_end(jim);
        // TODO: record_tests() must patch the existing json files instead of overwritting them
        let json_path = temp_sprintf(c!("%s/%s.json"), test_folder, case_name);
        if !write_entire_file(json_path, (*jim).sink as *const c_void, (*jim).sink_count) { return None; }
    }
    // TODO: the record operation should probably also print the report
    Some(())
}

pub unsafe fn replay_tests(
    // TODO: The Inputs and the Outputs want to be their own entity. But what should they be called?
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target],
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder, reports: *mut Array<Report>, stats_by_target: *mut Array<Stats>, jimp: *mut Jimp,
) -> Option<()> {
    // TODO: memory leak
    // We should probably pass it along with all the Outputs
    let mut target_outcomes_table: Array<(Target, Outcome)> = zeroed();

    // TODO: Parallelize the test runner.
    // Probably using `cmd_run_async_and_reset`.
    // Also don't forget to add the `-j` flag.
    for i in 0..cases.len() {
        let case_name = (*cases)[i];
        let mut report = Report {
            name: case_name,
            statuses: zeroed(),
        };
        let json_path = temp_sprintf(c!("%s/%s.json"), test_folder, case_name);
        if file_exists(json_path)? {
            // TODO: file may stop existing between file_exists() and read_entire_file() cools
            // It would be much better if read_entire_file() returned the reason of failure so
            // it's easy to check if it failed due to ENOENT, but that requires significant
            // changes to nob.h rn.
            (*sb).count = 0;
            read_entire_file(json_path, sb)?;
            jimp_begin(jimp, json_path, (*sb).items, (*sb).count);
            target_outcomes_table.count = 0;
            jimp_object_begin(jimp)?;
            while let Some(()) = jimp_object_member(jimp) {
                if let Some(target) = target_by_name((*jimp).string) {
                    let outcome: Outcome = outcome_deserialize(jimp)?;
                    da_append(&mut target_outcomes_table, (target, outcome));
                } else {
                    jimp_unknown_member(jimp);
                    return None;
                }
            }
            jimp_object_end(jimp)?;
        }
        for j in 0..targets.len() {
            let target = (*targets)[j];
            let expected_outcome = slice_lookup(
                da_slice(target_outcomes_table),
                |(key, outcome), target| if key == target { Some(outcome) } else { None },
                target
            );
            let actual_outcome = execute_test(
                // Inputs
                test_folder, case_name, target,
                // Outputs
                cmd, sb,
            )?;
            let expected = if let Some(expected_outcome) = expected_outcome {
                expected_outcome.status == actual_outcome.status &&
                    strcmp(expected_outcome.stdout, actual_outcome.stdout) == 0
            } else {
                false
            };
            da_append(&mut report.statuses, (actual_outcome.status, expected));
        }
        da_append(reports, report);
    }

    // TODO: generate HTML reports and deploy them somewhere automatically

    // TODO: report summary should take into account expected vs unexpected
    for j in 0..targets.len() {
        let mut stats: Stats = zeroed();
        for i in 0..(*reports).count {
            let report = *(*reports).items.add(i);
            match *report.statuses.items.add(j) {
                (OutcomeStatus::RunSuccess, true)  => stats.expected_ks   += 1,
                (OutcomeStatus::RunSuccess, false) => stats.unexpected_ks += 1,
                (OutcomeStatus::BuildFail,  true)  => stats.expected_bs   += 1,
                (OutcomeStatus::BuildFail,  false) => stats.unexpected_bs += 1,
                (OutcomeStatus::RunFail,    true)  => stats.expected_rs   += 1,
                (OutcomeStatus::RunFail,    false) => stats.unexpected_rs += 1,
            }
        }
        da_append(stats_by_target, stats);
    }

    let mut row_width = 0;
    for i in 0..(*reports).count {
        let report = *(*reports).items.add(i);
        row_width = cmp::max(row_width, strlen(report.name));
    }

    let mut col_width = 0;
    for j in 0..targets.len() {
        let target = (*targets)[j];
        let width = 2*(j + 1) + strlen(name_of_target(target).unwrap());
        col_width = cmp::max(col_width, width);
    }

    print_legend(row_width);
    printf(c!("\n"));
    print_top_labels(targets, da_slice(*stats_by_target), row_width, col_width);
    for i in 0..(*reports).count {
        let report = *(*reports).items.add(i);
        printf(c!("%*s:"), row_width, report.name);
        for j in 0..report.statuses.count {
            match *report.statuses.items.add(j) {
                (OutcomeStatus::RunSuccess, true)   => printf(c!(" %sK%s"), K_EXPECTED,   RESET),
                (OutcomeStatus::RunSuccess, false)  => printf(c!(" %sK%s"), K_UNEXPECTED, RESET),
                (OutcomeStatus::BuildFail,  true)   => printf(c!(" %sB%s"), B_EXPECTED,   RESET),
                (OutcomeStatus::BuildFail,  false)  => printf(c!(" %sB%s"), B_UNEXPECTED, RESET),
                (OutcomeStatus::RunFail,    true)   => printf(c!(" %sR%s"), R_EXPECTED,   RESET),
                (OutcomeStatus::RunFail,    false)  => printf(c!(" %sR%s"), R_UNEXPECTED, RESET),
            };
        }
        printf(c!("\n"));
    }
    print_bottom_labels(targets, da_slice(*stats_by_target), row_width, col_width);
    printf(c!("\n"));
    print_legend(row_width);

    Some(())
}

pub unsafe fn main(argc: i32, argv: *mut*mut c_char) -> Option<()> {
    let target_flags = flag_list(c!("t"), c!("Compilation targets to test on."));
    let list_targets = flag_bool(c!("tlist"), false, c!("Print the list of compilation targets"));
    let cases_flags  = flag_list(c!("c"), c!("Test cases"));
    let list_cases   = flag_bool(c!("clist"), false, c!("Print the list of test cases"));
    let test_folder  = flag_str(c!("dir"), c!("./tests/"), c!("Test folder"));
    let help         = flag_bool(c!("help"), false, c!("Print this help message"));
    let record       = flag_bool(c!("record"), false, c!("Record test cases instead of replaying them"));
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

    let mut sb: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();
    let mut jim: Jim = zeroed();
    let mut jimp: Jimp = zeroed();
    let mut reports: Array<Report> = zeroed();
    let mut stats_by_target: Array<Stats> = zeroed();

    let mut targets: Array<Target> = zeroed();
    if *list_targets || (*target_flags).count == 0 {
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
                fprintf(stderr(), c!("ERROR: unknown target `%s`\n"), target_name);
                return None;
            }
        }
    }

    if *list_targets {
        fprintf(stderr(), c!("Compilation targets:\n"));
        for i in 0..targets.count {
            let target = *targets.items.add(i);
            fprintf(stderr(), c!("    %s\n"), name_of_target(target).unwrap());
        }
        return Some(());
    }

    let mut cases: Array<*const c_char> = zeroed();
    if *list_cases || (*cases_flags).count == 0 {
        let mut case_files: File_Paths = zeroed();
        if !read_entire_dir(*test_folder, &mut case_files) { return None; } // TODO: memory leak. The file names are strduped to temp, but the File_Paths dynamic array itself is still allocated on the heap
        qsort(case_files.items as *mut c_void, case_files.count, size_of::<*const c_char>(), compar_cstr);

        for i in 0..case_files.count {
            let case_file = *case_files.items.add(i);
            if *case_file == '.' as c_char { continue; }
            let Some(case_name) = temp_strip_suffix(case_file, c!(".b")) else { continue; };
            da_append(&mut cases, case_name);
        }
    } else {
        for i in 0..(*cases_flags).count {
            let case_name = *(*cases_flags).items.add(i);
            da_append(&mut cases, case_name);
        }
    }

    if *list_cases {
        fprintf(stderr(), c!("Test cases:\n"));
        for i in 0..cases.count {
            let case = *cases.items.add(i);
            fprintf(stderr(), c!("    %s\n"), case);
        }
        return Some(());
    }

    if !mkdir_if_not_exists(GARBAGE_FOLDER) { return None; }

    if *record {
        record_tests(
            // Inputs
            *test_folder, da_slice(cases), da_slice(targets),
            // Outputs
            &mut cmd, &mut sb, &mut jim,
        )?;
    } else {
        replay_tests(
            // Inputs
            *test_folder, da_slice(cases), da_slice(targets),
            // Outputs
            &mut cmd, &mut sb, &mut reports, &mut stats_by_target, &mut jimp,
        );
    }

    Some(())
}
