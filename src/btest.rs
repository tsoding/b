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
use crust::*;
use crust::libc::*;
use nob::*;
use targets::*;
use runner::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;
use jim::*;
use jimp::*;

const GARBAGE_FOLDER: *const c_char = c!("./build/tests/");

#[derive(Copy, Clone)]
pub enum TestState {
    Enabled,
    Disabled,
}

pub unsafe fn test_state_deserialize(jimp: *mut Jimp) -> Option<TestState> {
    jimp_string(jimp)?;
    if strcmp((*jimp).string, c!("Enabled")) == 0 {
        Some(TestState::Enabled)
    } else if strcmp((*jimp).string, c!("Disabled")) == 0 {
        Some(TestState::Disabled)
    } else {
        jimp_unknown_member(jimp); // TODO: jimp_unknown_member() is not appropriate here, but it works cause it reports on jimp->string
        None
    }
}

pub unsafe fn test_state_serialize(jim: *mut Jim, test_state: TestState) {
    match test_state {
        TestState::Enabled  => jim_string(jim, c!("Enabled")),
        TestState::Disabled => jim_string(jim, c!("Disabled")),
    }
}

#[derive(Copy, Clone)]
pub struct TestConfig {
    pub expected_stdout: *const c_char,
    pub state: TestState,
    pub comment: *const c_char,
}

pub unsafe fn test_config_serialize(jim: *mut Jim, test_config: TestConfig) {
    jim_object_begin(jim);
        jim_member_key(jim, c!("expected_stdout"));
        jim_string(jim, test_config.expected_stdout);
        jim_member_key(jim, c!("state"));
        test_state_serialize(jim, test_config.state);
        jim_member_key(jim, c!("comment"));
        jim_string(jim, test_config.comment);
    jim_object_end(jim);
}

pub unsafe fn test_config_deserialize(jimp: *mut Jimp) -> Option<TestConfig> {
    let mut test_config: TestConfig = zeroed();
    jimp_object_begin(jimp)?;
    while let Some(()) = jimp_object_member(jimp) {
        if strcmp((*jimp).string, c!("expected_stdout")) == 0 {
            jimp_string(jimp)?;
            test_config.expected_stdout = strdup((*jimp).string); // TODO: memory leak
        } else if strcmp((*jimp).string, c!("state")) == 0 {
            test_config.state = test_state_deserialize(jimp)?;
        } else if strcmp((*jimp).string, c!("comment")) == 0 {
            jimp_string(jimp)?;
            test_config.comment = strdup((*jimp).string); // TODO: memory leak
        } else {
            jimp_unknown_member(jimp);
            return None;
        }
    }
    jimp_object_end(jimp)?;
    Some(test_config)
}

#[derive(Copy, Clone)]
pub enum Outcome {
    /// The test didn't even manage to build
    BuildFail,
    /// The test built, but crashed at runtime
    RunFail,
    /// The test built, printed something and exited normally
    RunSuccess{stdout: *const c_char},
}

#[derive(Copy, Clone)]
pub enum ReportStatus {
    BuildFail,
    RunFail,
    Disabled,
    StdoutMismatch,
    OK,
}

#[derive(Copy, Clone)]
pub struct Report {
    pub name: *const c_char,
    pub statuses: Array<ReportStatus>,
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
        return Some(Outcome::BuildFail);
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

    if let None = run_result {
        Some(Outcome::RunFail)
    } else {
        Some(Outcome::RunSuccess{stdout: strdup((*sb).items)}) // TODO: memory leak
    }
}

pub unsafe fn usage() {
    fprintf(stderr(), c!("Usage: %s [OPTIONS]\n"), flag_program_name());
    fprintf(stderr(), c!("OPTIONS:\n"));
    flag_print_options(stderr());
}

pub unsafe extern "C" fn compar_cstr(a: *const c_void, b: *const c_void) -> c_int {
    strcmp(*(a as *const *const c_char), *(b as *const *const c_char))
}

// TODO: Each field of Stats corresponds to an enum value of ReportStatus.
// It would be great if this code failed on any change of ReportStatus.
#[derive(Clone, Copy)]
pub struct ReportStats {
    build_fail: usize,
    run_fail: usize,
    disabled: usize,
    stdout_mismatch: usize,
    ok: usize,
}

const RESET:  *const c_char = c!("\x1b[0m");
const GREEN:  *const c_char = c!("\x1b[32m");
const YELLOW: *const c_char = c!("\x1b[33m");
const GREY:   *const c_char = c!("\x1b[90m");
const RED:    *const c_char = c!("\x1b[31m");

pub unsafe fn print_legend(row_width: usize) {
    printf(c!("%*s%sK%s - passed      %sX%s - unexpected stdout\n"), row_width + 2, c!(""), GREEN,  RESET, YELLOW, RESET);
    printf(c!("%*s%sB%s - build fail  %sR%s - runtime fail\n"),      row_width + 2, c!(""), RED,    RESET, RED,    RESET);
    printf(c!("%*s%s-%s - disabled\n"),                              row_width + 2, c!(""), GREY,   RESET);
}

pub unsafe fn print_report_stats(stats: ReportStats) {
    printf(c!(" %sK%s: %-3zu"), GREEN,  RESET, stats.ok);
    printf(c!(" %sX%s: %-3zu"), YELLOW, RESET, stats.stdout_mismatch);
    printf(c!(" %sB%s: %-3zu"), RED,    RESET, stats.build_fail);
    printf(c!(" %sR%s: %-3zu"), RED,    RESET, stats.run_fail);
    printf(c!(" %s-%s: %-3zu"), GREY,   RESET, stats.disabled);
    printf(c!("\n"));
}

pub unsafe fn print_top_labels(targets: *const [Target], stats_by_target: *const [ReportStats], row_width: usize, col_width: usize) {
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
        print_report_stats(stats)
    }
}

pub unsafe fn print_bottom_labels(targets: *const [Target], stats_by_target: *const [ReportStats], row_width: usize, col_width: usize) {
    assert!(targets.len() == stats_by_target.len());
    for j in (0..targets.len()).rev() {
        let target = (*targets)[j];
        let stats = (*stats_by_target)[j];
        printf(c!("%*s"), row_width + 2, c!(""));
        for _ in 0..j {
            printf(c!("│ "));
        }
        printf(c!("└─%-*s"), col_width - 2*j, name_of_target(target).unwrap());
        print_report_stats(stats)
    }
}

pub unsafe fn record_tests(
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], bat: *mut Bat,
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder,
    reports: *mut Array<Report>, stats_by_target: *mut Array<ReportStats>,
) -> Option<()> {
    // TODO: Parallelize the test runner.
    // Probably using `cmd_run_async_and_reset`.
    // Also don't forget to add the `-j` flag.
    for i in 0..cases.len() {
        let case_name = (*cases)[i];
        let mut report = Report {
            name: case_name,
            statuses: zeroed(),
        };

        let target_test_config_table = if let Some(found) = assoc_lookup_cstr_mut(da_slice(*bat), case_name) {
            found
        } else {
            da_append(bat, (case_name, zeroed()));
            &mut (*da_last_mut(bat).unwrap()).1
        };

        for j in 0..targets.len() {
            let target = (*targets)[j];
            if let Some(test_config) = assoc_lookup_mut(da_slice(*target_test_config_table), &target) {
                match (*test_config).state {
                    TestState::Enabled => {
                        let outcome = execute_test(
                            // Inputs
                            test_folder, case_name, target,
                            // Outputs
                            cmd, sb,
                        )?;
                        match outcome {
                            Outcome::BuildFail => da_append(&mut report.statuses, ReportStatus::BuildFail),
                            Outcome::RunFail   => da_append(&mut report.statuses, ReportStatus::RunFail),
                            Outcome::RunSuccess{stdout} => {
                                (*test_config).expected_stdout = stdout;
                                da_append(&mut report.statuses, ReportStatus::OK);
                            }
                        }
                    }
                    TestState::Disabled => da_append(&mut report.statuses, ReportStatus::Disabled),
                }
            } else {
                let outcome = execute_test(
                    // Inputs
                    test_folder, case_name, target,
                    // Outputs
                    cmd, sb,
                )?;
                match outcome {
                    Outcome::BuildFail => {
                        let new_test_config = TestConfig {
                            expected_stdout: c!(""),
                            state: TestState::Enabled,
                            comment: c!("Failed to build on record"),
                        };
                        da_append(target_test_config_table, (target, new_test_config));
                        da_append(&mut report.statuses, ReportStatus::BuildFail)
                    },
                    Outcome::RunFail => {
                        let new_test_config = TestConfig {
                            expected_stdout: c!(""),
                            state: TestState::Enabled,
                            comment: c!("Failed to run on record"),
                        };
                        da_append(target_test_config_table, (target, new_test_config));
                        da_append(&mut report.statuses, ReportStatus::RunFail)
                    }
                    Outcome::RunSuccess{stdout} => {
                        let new_test_config = TestConfig {
                            expected_stdout: stdout,
                            state: TestState::Enabled,
                            comment: c!(""),
                        };
                        da_append(target_test_config_table, (target, new_test_config));
                        da_append(&mut report.statuses, ReportStatus::OK);
                    }
                }
            }
        }
        da_append(reports, report);
    }

    collect_stats_by_target(targets, da_slice(*reports), stats_by_target);
    generate_report(da_slice(*reports), da_slice(*stats_by_target), targets);

    Some(())
}

pub unsafe fn collect_stats_by_target(targets: *const [Target], reports: *const [Report], stats_by_target: *mut Array<ReportStats>) {
    for j in 0..targets.len() {
        let mut stats: ReportStats = zeroed();
        for i in 0..reports.len() {
            let report = (*reports)[i];
            match *report.statuses.items.add(j) {
                ReportStatus::BuildFail      => stats.build_fail      += 1,
                ReportStatus::RunFail        => stats.run_fail        += 1,
                ReportStatus::Disabled       => stats.disabled        += 1,
                ReportStatus::StdoutMismatch => stats.stdout_mismatch += 1,
                ReportStatus::OK             => stats.ok              += 1,
            }
        }
        da_append(stats_by_target, stats);
    }
}

pub unsafe fn generate_report(reports: *const [Report], stats_by_target: *const [ReportStats], targets: *const [Target]) {
    let mut row_width = 0;
    for i in 0..reports.len() {
        let report = (*reports)[i];
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
    print_top_labels(targets, stats_by_target, row_width, col_width);
    for i in 0..reports.len() {
        let report = (*reports)[i];
        printf(c!("%*s:"), row_width, report.name);
        for j in 0..report.statuses.count {
            match *report.statuses.items.add(j) {
                ReportStatus::OK             => printf(c!(" %sK%s"), GREEN,  RESET),
                ReportStatus::StdoutMismatch => printf(c!(" %sX%s"), YELLOW, RESET),
                ReportStatus::BuildFail      => printf(c!(" %sB%s"), RED,    RESET),
                ReportStatus::RunFail        => printf(c!(" %sR%s"), RED,    RESET),
                ReportStatus::Disabled       => printf(c!(" %s-%s"), GREY,   RESET),
            };
        }
        printf(c!("\n"));
    }
    print_bottom_labels(targets, stats_by_target, row_width, col_width);
    printf(c!("\n"));
    print_legend(row_width);
}

type Bat = Array<(*const c_char, Array<(Target, TestConfig)>)>; // (Big Ass Table)

pub unsafe fn load_bat_from_json_file_if_exists(
    json_path: *const c_char,
    sb: *mut String_Builder, jimp: *mut Jimp
) -> Option<Bat> {
    let mut bat: Bat = zeroed();
    if file_exists(json_path)? {
        // TODO: file may stop existing between file_exists() and read_entire_file() cools
        // It would be much better if read_entire_file() returned the reason of failure so
        // it's easy to check if it failed due to ENOENT, but that requires significant
        // changes to nob.h rn.
        (*sb).count = 0;
        read_entire_file(json_path, sb)?;

        jimp_begin(jimp, json_path, (*sb).items, (*sb).count);

        jimp_object_begin(jimp)?;
        while let Some(()) = jimp_object_member(jimp) {
            let case_name = strdup((*jimp).string); // TODO: memory leak
            let mut target_test_config_table: Array<(Target, TestConfig)> = zeroed();
            jimp_object_begin(jimp)?;
            while let Some(()) = jimp_object_member(jimp) {
                if let Some(target) = target_by_name((*jimp).string) {
                    let test_config: TestConfig = test_config_deserialize(jimp)?;
                    da_append(&mut target_test_config_table, (target, test_config));
                } else {
                    jimp_unknown_member(jimp);
                    return None;
                }
            }
            jimp_object_end(jimp)?;
            da_append(&mut bat, (case_name, target_test_config_table));
        }
        jimp_object_end(jimp)?;
    }
    Some(bat)
}

pub unsafe fn save_bat_to_json_file(
    json_path: *const c_char, bat: Bat,
    jim: *mut Jim,
) -> Option<()> {
    jim_begin(jim);
    jim_object_begin(jim);
    for i in 0..bat.count {
        let (case_name, target_test_config_table) = *bat.items.add(i);
        jim_member_key(jim, case_name);
        jim_object_begin(jim);
        for j in 0..target_test_config_table.count {
            let (target, outcome) = *target_test_config_table.items.add(j);
            jim_member_key(jim, name_of_target(target).unwrap());
            test_config_serialize(jim, outcome);
        }
        jim_object_end(jim);
    }
    jim_object_end(jim);

    write_entire_file(json_path, (*jim).sink as *const c_void, (*jim).sink_count)
}

pub unsafe fn replay_tests(
    // TODO: The Inputs and the Outputs want to be their own entity. But what should they be called?
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], bat: Bat,
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder, reports: *mut Array<Report>, stats_by_target: *mut Array<ReportStats>, jim: *mut Jim,
) -> Option<()> {

    // TODO: Parallelize the test runner.
    // Probably using `cmd_run_async_and_reset`.
    // Also don't forget to add the `-j` flag.
    for i in 0..cases.len() {
        let case_name = (*cases)[i];
        let mut report = Report {
            name: case_name,
            statuses: zeroed(),
        };

        let target_test_config_table = if let Some(found) = assoc_lookup_cstr(da_slice(bat), case_name) {
            *found
        } else {
            zeroed()
        };

        for j in 0..targets.len() {
            let target = (*targets)[j];
            if let Some(test_config) = assoc_lookup(da_slice(target_test_config_table), &target) {
                match (*test_config).state {
                    TestState::Enabled => {
                        let outcome = execute_test(
                            // Inputs
                            test_folder, case_name, target,
                            // Outputs
                            cmd, sb,
                        )?;
                        match outcome {
                            Outcome::RunSuccess{stdout} =>
                                if strcmp((*test_config).expected_stdout, stdout) != 0 {
                                    fprintf(stderr(), c!("UNEXPECTED OUTCOME!!!\n"));
                                    jim_begin(jim);
                                    jim_string(jim, (*test_config).expected_stdout);
                                    fprintf(stderr(), c!("EXPECTED: %.*s\n"), (*jim).sink_count, (*jim).sink);
                                    jim_begin(jim);
                                    jim_string(jim, stdout);
                                    fprintf(stderr(), c!("ACTUAL:   %.*s\n"), (*jim).sink_count, (*jim).sink);
                                    da_append(&mut report.statuses, ReportStatus::StdoutMismatch);
                                } else {
                                    da_append(&mut report.statuses, ReportStatus::OK);
                                },
                            Outcome::BuildFail => da_append(&mut report.statuses, ReportStatus::BuildFail),
                            Outcome::RunFail   => da_append(&mut report.statuses, ReportStatus::RunFail),
                        }
                    }
                    TestState::Disabled => da_append(&mut report.statuses, ReportStatus::Disabled),
                }
            } else {
                let outcome = execute_test(
                    // Inputs
                    test_folder, case_name, target,
                    // Outputs
                    cmd, sb,
                )?;

                match outcome {
                    Outcome::RunSuccess{..} => {
                        fprintf(stderr(), c!("UNEXPECTED OUTCOME!!! The outcome was never recorded. Please use -record flag to record what is expected for this test case at this target\n"));
                        da_append(&mut report.statuses, ReportStatus::StdoutMismatch);
                    }
                    Outcome::BuildFail => da_append(&mut report.statuses, ReportStatus::BuildFail),
                    Outcome::RunFail   => da_append(&mut report.statuses, ReportStatus::RunFail),
                }
            }
        }
        da_append(reports, report);
    }

    collect_stats_by_target(targets, da_slice(*reports), stats_by_target);
    generate_report(da_slice(*reports), da_slice(*stats_by_target), targets);

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
    jim.pp = 4;
    let mut jimp: Jimp = zeroed();
    let mut reports: Array<Report> = zeroed();
    let mut stats_by_target: Array<ReportStats> = zeroed();

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

    let json_path = c!("tests.json");
    if *record {
        let mut bat = load_bat_from_json_file_if_exists(json_path, &mut sb, &mut jimp)?;
        record_tests(
            // Inputs
            *test_folder, da_slice(cases), da_slice(targets), &mut bat,
            // Outputs
            &mut cmd, &mut sb, &mut reports, &mut stats_by_target,
        )?;
        save_bat_to_json_file(json_path, bat, &mut jim)?;
    } else {
        let bat = load_bat_from_json_file_if_exists(json_path, &mut sb, &mut jimp)?;
        replay_tests(
            // Inputs
            *test_folder, da_slice(cases), da_slice(targets), bat,
            // Outputs
            &mut cmd, &mut sb, &mut reports, &mut stats_by_target, &mut jim,
        );
    }

    Some(())
}

// TODO: generate HTML reports and deploy them somewhere automatically
// TODO: Make btest test historical mode
