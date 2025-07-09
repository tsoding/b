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
use glob::*;
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

enum_with_order! {
    #[derive(Copy, Clone)]
    enum ReportStatus in REPORT_STATUS_ORDER {
        OK,
        NeverRecorded,
        StdoutMismatch,
        BuildFail,
        RunFail,
        Disabled,
    }
}

impl ReportStatus {
    fn letter(self) -> *const c_char {
        match self {
            ReportStatus::OK             => c!("K"),
            ReportStatus::NeverRecorded  => c!("K"),
            ReportStatus::StdoutMismatch => c!("K"),
            ReportStatus::BuildFail      => c!("B"),
            ReportStatus::RunFail        => c!("R"),
            ReportStatus::Disabled       => c!("-"),
        }
    }

    fn color(self) -> *const c_char {
        match self {
            ReportStatus::OK             => GREEN,
            ReportStatus::NeverRecorded  => BLUE,
            ReportStatus::StdoutMismatch => RED,
            ReportStatus::BuildFail      => RED,
            ReportStatus::RunFail        => RED,
            ReportStatus::Disabled       => GREY,
        }
    }

    fn description(self) -> *const c_char {
        match self {
            ReportStatus::OK             => c!("passed"),
            ReportStatus::NeverRecorded  => c!("stdout is not recorded"),
            ReportStatus::StdoutMismatch => c!("unexpected stdout"),
            ReportStatus::BuildFail      => c!("build fail"),
            ReportStatus::RunFail        => c!("runtime error"),
            ReportStatus::Disabled       => c!("disabled"),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Report {
    pub name: *const c_char,
    pub statuses: Array<ReportStatus>,
}

pub unsafe fn execute_test(
    // Inputs
    test_folder: *const c_char, name: *const c_char, target: Target, opt: bool,
    // Outputs
    cmd: *mut Cmd, sb: *mut String_Builder,
) -> Option<Outcome> {
    // TODO: add timeouts for running and building in case they go into infinite loop or something
    let input_path = temp_sprintf(c!("%s/%s.b"), test_folder, name);
    let program_path = temp_sprintf(c!("%s/%s.%s"), GARBAGE_FOLDER, name, match target {
        Target::Gas_x86_64_Darwin   => c!("gas-x86_64-darwin"),
        Target::Gas_AArch64_Linux   => c!("gas-aarch64-linux"),
        Target::Gas_AArch64_Darwin  => c!("gas-aarch64-darwin"),
        Target::Gas_x86_64_Linux    => c!("gas-x86_64-linux"),
        Target::Gas_x86_64_Windows  => c!("exe"),
        Target::Uxn                 => c!("rom"),
        Target::Mos6502             => c!("6502"),
    });
    let stdout_path = temp_sprintf(c!("%s/%s.%s.stdout.txt"), GARBAGE_FOLDER, name, target.name());
    cmd_append! {
        cmd,
        c!("./build/b"),
        input_path,
        c!("-t"), target.name(),
        c!("-o"), program_path,
    }
    if opt {
        da_append(cmd, c!("-O"));
    }
    if !cmd_run_sync_and_reset(cmd) {
        return Some(Outcome::BuildFail);
    }
    let run_result = match target {
        Target::Gas_AArch64_Linux   => runner::gas_aarch64_linux::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_AArch64_Darwin  => runner::gas_aarch64_darwin::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_x86_64_Linux    => runner::gas_x86_64_linux::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_x86_64_Windows  => runner::gas_x86_64_windows::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Gas_x86_64_Darwin   => runner::gas_x86_64_darwin::run(cmd, program_path, &[], Some(stdout_path)),
        Target::Uxn                 => runner::uxn::run(cmd, c!("uxncli"), program_path, &[], Some(stdout_path)),
        Target::Mos6502             => runner::mos6502::run(sb, Config {
            load_offset: DEFAULT_LOAD_OFFSET
        }, program_path, Some(stdout_path)),
    };

    (*sb).count = 0;
    read_entire_file(stdout_path, sb)?; // Should always succeed, but may fail if stdout_path is a directory for instance.
    da_append(sb, 0);                   // NULL-terminating the stdout
    printf(c!("%s"), (*sb).items);      // Forward stdout for diagnostic purposes

    if run_result.is_none() {
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

#[derive(Clone, Copy)]
pub struct ReportStats {
    entries: [usize; REPORT_STATUS_ORDER.len()]
}

const RESET:  *const c_char = c!("\x1b[0m");
const GREEN:  *const c_char = c!("\x1b[32m");
const _YELLOW: *const c_char = c!("\x1b[33m");
const GREY:   *const c_char = c!("\x1b[90m");
const RED:    *const c_char = c!("\x1b[31m");
const BLUE:   *const c_char = c!("\x1b[94m");

pub unsafe fn print_legend(row_width: usize) {
    for i in 0..REPORT_STATUS_ORDER.len() {
        let status = (*REPORT_STATUS_ORDER)[i];
        printf(c!("%*s%s%s%s - %s\n"), row_width + 2, c!(""), status.color(), status.letter(), RESET, status.description());
    }
}

pub unsafe fn print_report_stats(stats: ReportStats) {
    for i in 0..REPORT_STATUS_ORDER.len() {
        let status = (*REPORT_STATUS_ORDER)[i];
        printf(c!(" %s%s%s: %-3zu"), status.color(), status.letter(), RESET, stats.entries[i]);
    }
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
        printf(c!("┌─%-*s"), col_width - 2*j, target.name());
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
        printf(c!("└─%-*s"), col_width - 2*j, target.name());
        print_report_stats(stats)
    }
}

pub unsafe fn matches_glob(pattern: *const c_char, text: *const c_char) -> Option<bool> {
    let mark = temp_save();
    let result = match glob_utf8(pattern, text) {
        Glob_Result::MATCHED        => Ok(true),
        Glob_Result::UNMATCHED      => Ok(false),
        Glob_Result::OOM_ERROR      => Err(c!("out of memory")),
        Glob_Result::ENCODING_ERROR => Err(c!("encoding error")),
        Glob_Result::SYNTAX_ERROR   => Err(c!("syntax error")),
    };
    temp_rewind(mark);
    match result {
        Ok(result) => Some(result),
        Err(error) => {
            fprintf(stderr(), c!("ERROR: while matching pattern `%s`: %s\n"), pattern, error);
            None
        }
    }
}

pub unsafe fn record_tests(
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], bat: *mut Bat, opt: bool,
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
                            test_folder, case_name, target, opt,
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
                    test_folder, case_name, target, opt,
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
            stats.entries[*report.statuses.items.add(j) as usize] += 1;
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
        let width = 2*(j + 1) + strlen(target.name());
        col_width = cmp::max(col_width, width);
    }

    print_legend(row_width);
    printf(c!("\n"));
    print_top_labels(targets, stats_by_target, row_width, col_width);
    for i in 0..reports.len() {
        let report = (*reports)[i];
        printf(c!("%*s:"), row_width, report.name);
        for j in 0..report.statuses.count {
            let status = *report.statuses.items.add(j);
            printf(c!(" %s%s%s"), status.color(), status.letter(), RESET);
        }
        printf(c!("\n"));
    }
    print_bottom_labels(targets, stats_by_target, row_width, col_width);
    printf(c!("\n"));
    print_legend(row_width);
}

type Bat = Array<(*const c_char, Array<(Target, TestConfig)>)>; // (Big Ass Table)

pub unsafe fn load_bat_from_json_file_if_exists(
    json_path: *const c_char, test_folder: *const c_char,
    sb: *mut String_Builder, jimp: *mut Jimp
) -> Option<Bat> {
    let mut bat: Bat = zeroed();
    if file_exists(json_path)? {
        printf(c!("INFO: loading file %s...\n"), json_path);
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
            let case_start = (*jimp).token_start;
            let mut target_test_config_table: Array<(Target, TestConfig)> = zeroed();
            jimp_object_begin(jimp)?;
            while let Some(()) = jimp_object_member(jimp) {
                let target_name = strdup((*jimp).string); // TODO: memory leak
                let target = Target::by_name(target_name);
                let target_start = (*jimp).token_start;
                let test_config: TestConfig = test_config_deserialize(jimp)?;
                if let Some(target) = target {
                    da_append(&mut target_test_config_table, (target, test_config));
                } else {
                    (*jimp).token_start = target_start;
                    // TODO: memory leak, we are dropping the whole test_config here
                    jimp_diagf(jimp, c!("WARNING: Unknown target: %s. Ignoring it...\n"), target_name);
                }
            }
            jimp_object_end(jimp)?;

            let case_path = temp_sprintf(c!("%s/%s.b"), test_folder, case_name);
            if file_exists(case_path)? {
                da_append(&mut bat, (case_name, target_test_config_table));
            } else {
                (*jimp).token_start = case_start;
                // TODO: memory leak, we are dropping the whole target_test_config_table here
                jimp_diagf(jimp, c!("WARNING: %s does not exist. Ignoring %s test case...\n"), case_path, case_name);
            }
        }
        jimp_object_end(jimp)?;
    } else {
        printf(c!("INFO: %s doesn't exist. Nothing to load.\n"), json_path);
    }
    Some(bat)
}

pub unsafe fn save_bat_to_json_file(
    json_path: *const c_char, bat: Bat,
    jim: *mut Jim,
) -> Option<()> {
    printf(c!("INFO: saving file %s...\n"), json_path);
    jim_begin(jim);
    jim_object_begin(jim);
    for i in 0..bat.count {
        let (case_name, target_test_config_table) = *bat.items.add(i);
        jim_member_key(jim, case_name);
        jim_object_begin(jim);
        for j in 0..target_test_config_table.count {
            let (target, outcome) = *target_test_config_table.items.add(j);
            jim_member_key(jim, target.name());
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
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], bat: Bat, opt: bool,
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
                            test_folder, case_name, target, opt,
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
                    test_folder, case_name, target, opt,
                    // Outputs
                    cmd, sb,
                )?;

                match outcome {
                    Outcome::RunSuccess{..} => {
                        fprintf(stderr(), c!("UNEXPECTED OUTCOME!!! The outcome was never recorded. Please use -record flag to record what is expected for this test case at this target\n"));
                        da_append(&mut report.statuses, ReportStatus::NeverRecorded);
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

enum_with_order! {
    #[derive(Copy, Clone)]
    enum Action in ACTION_ORDER {
        Replay,
        Record,
        Prune,
    }
}

impl Action {
    unsafe fn name(self) -> *const c_char {
        match self {
            Self::Replay => c!("replay"),
            Self::Record => c!("record"),
            Self::Prune  => c!("prune"),
        }
    }

    unsafe fn from_name(name: *const c_char) -> Option<Self> {
        for i in 0..ACTION_ORDER.len() {
            let action = (*ACTION_ORDER)[i];
            if strcmp(name, action.name()) == 0 {
                return Some(action)
            }
        }
        None
    }
}

pub unsafe fn main(argc: i32, argv: *mut*mut c_char) -> Option<()> {
    let default_action = Action::Replay;

    let target_flags         = flag_list(c!("t"), c!("Compilation targets to select for testing. Can be a glob pattern."));
    let exclude_target_flags = flag_list(c!("xt"), c!("Compilation targets to exclude from selected ones. Can be a glob pattern"));
    let list_targets         = flag_bool(c!("tlist"), false, c!("Print the list of selected compilation targets."));

    let cases_flags          = flag_list(c!("c"), c!("Test cases to select for testing. Can be a glob pattern."));
    let exclude_cases_flags  = flag_list(c!("xc"), c!("Test cases to exclude from selected ones. Can be a glob pattern"));
    let list_cases           = flag_bool(c!("clist"), false, c!("Print the list of selected test cases"));

    let action_flag          = flag_str(c!("a"), default_action.name(), c!("Action to perform. Use -alist to get the list of available actions"));
    let list_actions         = flag_bool(c!("alist"), false, c!("Print the list of all available actions."));
    let record               = flag_bool(c!("record"), false, strdup(temp_sprintf(c!("DEPRECATED! Please use `-%s %s` flag instead."), flag_name(action_flag), Action::Record.name()))); // TODO: memory leak

    let opt                  = flag_bool(c!("O"), false, c!("Enable compiler optimizations"));

    let test_folder          = flag_str(c!("dir"), c!("./tests/"), c!("Test folder"));
    let help                 = flag_bool(c!("help"), false, c!("Print this help message"));

    if !flag_parse(argc, argv) {
        usage();
        flag_print_error(stderr());
        return None;
    }

    if *help {
        usage();
        return Some(());
    }

    let Some(action) = Action::from_name(*action_flag) else {
        fprintf(stderr(), c!("ERROR: unknown action `%s`\n"), *action_flag);
        return None;
    };

    let json_path = c!("tests.json");

    if *list_actions {
        fprintf(stderr(), c!("Available actions:\n"));
        let mut width = 0;
        for i in 0..ACTION_ORDER.len() {
            width = cmp::max(width, strlen((*ACTION_ORDER)[i].name()));
        }
        for i in 0..ACTION_ORDER.len() {
            let action = (*ACTION_ORDER)[i];
            match action {
                Action::Replay => {
                    printf(c!("  %*s - Replay the selected Test Matrix slice with expected outputs from %s.\n"), width, action.name(), json_path);
                }
                Action::Record => {
                    printf(c!("  %*s - Record the selected Test Matrix slice into %s.\n"), width, action.name(), json_path);
                }
                Action::Prune  => {
                    printf(c!("  %*s - Prune all the recordings from %s that are outside of the selected Test Matrix slice.\n"), width, action.name(), json_path);
                    printf(c!("  %*s   Useful when you delete targets or test cases. Just delete a target or a test case and\n"), width, c!(""));
                    printf(c!("  %*s   run `%s -%s %s` without any additional flags.\n"), width, c!(""), flag_program_name(), flag_name(action_flag), Action::Prune.name());
                }
            };
        }
        return Some(());
    }

    let mut sb: String_Builder = zeroed();
    let mut cmd: Cmd = zeroed();
    let mut jim: Jim = zeroed();
    jim.pp = 4;
    let mut jimp: Jimp = zeroed();
    let mut reports: Array<Report> = zeroed();
    let mut stats_by_target: Array<ReportStats> = zeroed();

    let mut selected_targets: Array<Target> = zeroed();
    if (*target_flags).count == 0 {
        da_append_many(&mut selected_targets, TARGET_ORDER);
    } else {
        for j in 0..(*target_flags).count {
            let mut added_anything = false;
            let pattern = *(*target_flags).items.add(j);
            for j in 0..TARGET_ORDER.len() {
                let target = (*TARGET_ORDER)[j];
                let name = target.name();
                if matches_glob(pattern, name)? {
                    da_append(&mut selected_targets, target);
                    added_anything = true;
                }
            }
            if !added_anything {
                fprintf(stderr(), c!("ERROR: unknown target `%s`\n"), pattern);
                return None;
            }
        }
    }
    let mut targets: Array<Target> = zeroed();
    for i in 0..selected_targets.count {
        let target = *selected_targets.items.add(i);
        let mut matches_any = false;
        'exclude: for j in 0..(*exclude_target_flags).count {
            let pattern = *(*exclude_target_flags).items.add(j);
            if matches_glob(pattern, target.name())? {
                matches_any = true;
                break 'exclude;
            }
        }
        if !matches_any {
            da_append(&mut targets, target);
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

    let mut selected_cases: Array<*const c_char> = zeroed();
    if (*cases_flags).count == 0 {
        selected_cases = all_cases;
    } else {
        for i in 0..(*cases_flags).count {
            let saved_count = selected_cases.count;
            let pattern = *(*cases_flags).items.add(i);
            for i in 0..all_cases.count {
                let case_name = *all_cases.items.add(i);
                if matches_glob(pattern, case_name)? {
                    da_append(&mut selected_cases, case_name);
                }
            }
            if selected_cases.count == saved_count {
                fprintf(stderr(), c!("ERROR: unknown test case `%s`\n"), pattern);
                return None;
            }
        }
    }
    let mut cases: Array<*const c_char> = zeroed();
    for i in 0..selected_cases.count {
        let case = *selected_cases.items.add(i);
        let mut matches_any = false;
        'exclude: for j in 0..(*exclude_cases_flags).count {
            let pattern = *(*exclude_cases_flags).items.add(j);
            if matches_glob(pattern, case)? {
                matches_any = true;
                break 'exclude;
            }
        }
        if !matches_any {
            da_append(&mut cases, case);
        }
    }

    // TODO: maybe merge -tlist and -clist outputs if they are provided together

    if *list_targets {
        fprintf(stderr(), c!("Compilation targets:\n"));
        for i in 0..targets.count {
            let target = *targets.items.add(i);
            fprintf(stderr(), c!("    %s\n"), target.name());
        }
        return Some(());
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
        fprintf(stderr(), c!("ERROR: Flag `-%s` is DEPRECATED! Please use `-%s %s` instead.\n"), flag_name(record), flag_name(action_flag), Action::Record.name());
        return None
    }

    match action {
        Action::Record => {
            let mut bat = load_bat_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            record_tests(
                // Inputs
                *test_folder, da_slice(cases), da_slice(targets), &mut bat, *opt,
                // Outputs
                &mut cmd, &mut sb, &mut reports, &mut stats_by_target,
            )?;
            save_bat_to_json_file(json_path, bat, &mut jim)?;
        }
        Action::Replay => {
            let bat = load_bat_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            replay_tests(
                // Inputs
                *test_folder, da_slice(cases), da_slice(targets), bat, *opt,
                // Outputs
                &mut cmd, &mut sb, &mut reports, &mut stats_by_target, &mut jim,
            );
        }
        Action::Prune => {
            let bat = load_bat_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            save_bat_to_json_file(json_path, bat, &mut jim)?;
        }
    }

    Some(())
}

// TODO: generate HTML reports and deploy them somewhere automatically
// TODO: Make btest test historical mode
