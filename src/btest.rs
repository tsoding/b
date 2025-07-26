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
pub mod flag;
pub mod glob;
pub mod jim;
pub mod jimp;
pub mod lexer;
pub mod codegen;
pub mod ir;

use core::ffi::*;
use core::cmp;
use core::mem::{zeroed, size_of};
use core::ptr;
use crust::libc::*;
use nob::*;
use targets::*;
use codegen::mos6502::{Config, DEFAULT_LOAD_OFFSET};
use flag::*;
use glob::*;
use jim::*;
use jimp::*;

const GARBAGE_FOLDER: *const c_char = c!("./build/tests/");

enum_with_order! {
    #[derive(Copy, Clone)]
    enum TestState in TEST_STATE_ORDER {
        Enabled,
        Disabled,
    }
}

impl TestState {
    unsafe fn name(self) -> *const c_char {
        match self {
            Self::Enabled  => c!("Enabled"),
            Self::Disabled => c!("Disabled"),
        }
    }

    unsafe fn from_name(name: *const c_char) -> Option<Self> {
        for i in 0..TEST_STATE_ORDER.len() {
            let state = (*TEST_STATE_ORDER)[i];
            if strcmp(state.name(), name) == 0 {
                return Some(state)
            }
        }
        None
    }
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
    test_folder: *const c_char, name: *const c_char, target: Target,
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
        // TODO: ILasm_Mono may collide with Gas_x86_64_Windows if we introduce parallel runner
        Target::ILasm_Mono          => c!("exe"),
        Target::ILasm_Core          => c!("dll"),
    });
    let stdout_path = temp_sprintf(c!("%s/%s.%s.stdout.txt"), GARBAGE_FOLDER, name, target.name());
    cmd_append! {
        cmd,
        if cfg!(target_os = "windows") {
            c!("./build/b.exe")
        } else {
            c!("./build/b")
        },
        input_path,
        c!("-t"), target.name(),
        c!("-o"), program_path,
    }
    if !cmd_run_sync_and_reset(cmd) {
        return Some(Outcome::BuildFail);
    }
    let run_result = match target {
        Target::Gas_AArch64_Linux   => codegen::gas_aarch64::run_program(cmd, program_path, &[], Some(stdout_path), Os::Linux),
        Target::Gas_AArch64_Darwin  => codegen::gas_aarch64::run_program(cmd, program_path, &[], Some(stdout_path), Os::Darwin),
        Target::Gas_x86_64_Linux    => codegen::gas_x86_64::run_program(cmd, program_path, &[], Some(stdout_path), Os::Linux),
        Target::Gas_x86_64_Windows  => codegen::gas_x86_64::run_program(cmd, program_path, &[], Some(stdout_path), Os::Windows),
        Target::Gas_x86_64_Darwin   => codegen::gas_x86_64::run_program(cmd, program_path, &[], Some(stdout_path), Os::Darwin),
        Target::Uxn                 => codegen::uxn::run_program(cmd, c!("uxncli"), program_path, &[], Some(stdout_path)),
        Target::Mos6502             => codegen::mos6502::run_program(sb, Config {
            load_offset: DEFAULT_LOAD_OFFSET
        }, program_path, Some(stdout_path)),
        Target::ILasm_Mono          => codegen::ilasm::run_program(cmd, program_path, &[], Some(stdout_path), true),
        Target::ILasm_Core          => codegen::ilasm::run_program(cmd, program_path, &[], Some(stdout_path), false),
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
    fprintf(stderr(), c!("B Compiler Testing Tool\n"));
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
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], tt: *mut TestTable,
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

        for j in 0..targets.len() {
            let target = (*targets)[j];
            if let Some(test_row) = test_table_find_row(tt, case_name, target) {
                match (*test_row).state {
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
                                (*test_row).expected_stdout = stdout;
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
                        da_append(tt, TestRow {
                            case_name,
                            target,
                            expected_stdout: c!(""),
                            state: TestState::Enabled,
                            comment: c!("Failed to build on record"),
                        });
                        da_append(&mut report.statuses, ReportStatus::BuildFail)
                    },
                    Outcome::RunFail => {
                        da_append(tt, TestRow {
                            case_name,
                            target,
                            expected_stdout: c!(""),
                            state: TestState::Enabled,
                            comment: c!("Failed to run on record"),
                        });
                        da_append(&mut report.statuses, ReportStatus::RunFail)
                    }
                    Outcome::RunSuccess{stdout} => {
                        da_append(tt, TestRow {
                            case_name,
                            target,
                            expected_stdout: stdout,
                            state: TestState::Enabled,
                            comment: c!(""),
                        });
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

pub struct TestRow {
    pub case_name: *const c_char,
    pub target: Target,
    pub expected_stdout: *const c_char,
    pub state: TestState,
    pub comment: *const c_char,
}

type TestTable = Array<TestRow>;

// TODO: test_table_find_row is O(n) which usually causes no problems on small arrays, but TestTable by the nature of the data
// it holds has a tendency to grow rather fast (the growth is O(Cases*Targets), basically every time we add a case it adds
// Targets amount of rows). We should invest into improving the performance of this operation rather soon. HashMap<K, V> is
// out of the question of course since we are torturing ourselves with Crust. We could implement our own HashMap or we could
// maybe sort this array on loading it from the JSON file and do Binary Search. Sorting has additional benefit of keeping
// the JSON file tidy.
pub unsafe fn test_table_find_row(tt: *mut TestTable, case_name: *const c_char, target: Target) -> Option<*mut TestRow> {
    for i in 0..(*tt).count {
        let row = (*tt).items.add(i);
        if (*row).target == target && strcmp((*row).case_name, case_name) == 0 {
            return Some(row)
        }
    }
    None
}

pub unsafe fn load_tt_from_json_file_if_exists(
    json_path: *const c_char, test_folder: *const c_char,
    sb: *mut String_Builder, jimp: *mut Jimp
) -> Option<TestTable> {
    let mut tt: TestTable = zeroed();
    if file_exists(json_path)? {
        printf(c!("INFO: loading file %s...\n"), json_path);
        // TODO: file may stop existing between file_exists() and read_entire_file() cools
        // It would be much better if read_entire_file() returned the reason of failure so
        // it's easy to check if it failed due to ENOENT, but that requires significant
        // changes to nob.h rn.
        (*sb).count = 0;
        read_entire_file(json_path, sb)?;

        jimp_begin(jimp, json_path, (*sb).items, (*sb).count);

        jimp_array_begin(jimp)?;
        'table: while jimp_array_item(jimp) {
            let saved_point = (*jimp).point;

            let mut case_name: *const c_char = ptr::null();
            let mut target: Option<Target> = None;
            let mut expected_stdout: *const c_char = c!("");
            let mut state = TestState::Enabled;
            let mut comment: *const c_char = c!("");

            jimp_object_begin(jimp)?;
            'row: while jimp_object_member(jimp) {
                if strcmp((*jimp).string, c!("case")) == 0 {
                    jimp_string(jimp)?;
                    case_name = strdup((*jimp).string); // TODO: memory leak
                    continue 'row;
                }
                if strcmp((*jimp).string, c!("target")) == 0 {
                    jimp_string(jimp)?;
                    if let Some(parsed_target) = Target::by_name((*jimp).string) {
                        target = Some(parsed_target);
                    } else {
                        jimp_diagf(jimp, c!("WARNING: invalid target name `%s`\n"), (*jimp).string);
                        jimp_diagf(jimp, c!("NOTE: Expected target values are:\n"));
                        for i in 0..TARGET_ORDER.len() {
                            jimp_diagf(jimp, c!("NOTE:  %s\n"), (*TARGET_ORDER)[i]);
                        }
                    }
                    continue 'row;
                }
                if strcmp((*jimp).string, c!("expected_stdout")) == 0 {
                    jimp_string(jimp)?;
                    expected_stdout = strdup((*jimp).string); // TODO: memory leak
                    continue 'row;
                }
                if strcmp((*jimp).string, c!("state")) == 0 {
                    jimp_string(jimp)?;
                    if let Some(parsed_state) = TestState::from_name((*jimp).string) {
                        state = parsed_state;
                    } else {
                        jimp_diagf(jimp, c!("WARNING: invalid state name `%s`\n"), (*jimp).string);
                        jimp_diagf(jimp, c!("NOTE: Expected state values are:\n"));
                        for i in 0..TEST_STATE_ORDER.len() {
                            jimp_diagf(jimp, c!("NOTE:  %s\n"), (*TEST_STATE_ORDER)[i]);
                        }
                    }
                    continue 'row;
                }
                if strcmp((*jimp).string, c!("comment")) == 0 {
                    jimp_string(jimp)?;
                    comment = strdup((*jimp).string); // TODO: memory leak
                    continue 'row;
                }

                jimp_diagf(jimp, c!("ERROR: unknown test row field `%s`\n"), (*jimp).string);
                return None;
            }
            jimp_object_end(jimp)?;

            let Some(target) = target else {
                (*jimp).token_start = saved_point;
                jimp_diagf(jimp, c!("ERROR: `target` is not defined for this test row. Ignoring the entire row...\n"));
                // TODO: memory leak, we are dropping the whole row here
                continue 'table;
            };

            if case_name.is_null() {
                (*jimp).token_start = saved_point;
                jimp_diagf(jimp, c!("ERROR: `case_name` is not defined for this test row. Ignoring the entire row...\n"));
                // TODO: memory leak, we are dropping the whole row here
                continue 'table;
            }

            if let Some(_existing_row) = test_table_find_row(&mut tt, case_name, target) {
                (*jimp).token_start = saved_point;
                // TODO: report the location of existing_row here as a NOTE
                // This requires keeping track of location in TestRow structure. Which requires location tracking capabilities
                // comparable to lexer.rs but in jim/jimp.
                jimp_diagf(jimp, c!("WARNING: Redefinition of the row case `%s`, target `%s`. We are using only the first definition. All the rest are gonna be prunned"), case_name, target.name());
                // TODO: memory leak, we are dropping the whole row here
                continue 'table;
            }

            let case_path = temp_sprintf(c!("%s/%s.b"), test_folder, case_name);
            if !file_exists(case_path)? {
                (*jimp).token_start = saved_point;
                jimp_diagf(jimp, c!("WARNING: %s does not exist. Ignoring case `%s`, target `%s` ...\n"), case_path, case_name, target.name());
                // TODO: memory leak, we are dropping the whole row here
                continue 'table;
            }

            da_append(&mut tt, TestRow {
                case_name,
                target,
                expected_stdout,
                state,
                comment,
            });
        }
        jimp_array_end(jimp)?;
    } else {
        printf(c!("INFO: %s doesn't exist. Nothing to load.\n"), json_path);
    }
    Some(tt)
}

pub unsafe fn save_tt_to_json_file(
    json_path: *const c_char, tt: TestTable,
    jim: *mut Jim,
) -> Option<()> {
    printf(c!("INFO: saving file %s...\n"), json_path);
    jim_begin(jim);
    jim_array_begin(jim);
    for i in 0..tt.count {
        let row = tt.items.add(i);

        jim_object_begin(jim);

        jim_member_key(jim, c!("case"));
        jim_string(jim, (*row).case_name);
        jim_member_key(jim, c!("target"));
        jim_string(jim, (*row).target.name());
        jim_member_key(jim, c!("expected_stdout"));
        jim_string(jim, (*row).expected_stdout);
        jim_member_key(jim, c!("state"));
        jim_string(jim, (*row).state.name());
        jim_member_key(jim, c!("comment"));
        jim_string(jim, (*row).comment);

        jim_object_end(jim);
    }
    jim_array_end(jim);

    write_entire_file(json_path, (*jim).sink as *const c_void, (*jim).sink_count)
}

pub unsafe fn replay_tests(
    // TODO: The Inputs and the Outputs want to be their own entity. But what should they be called?
    // Inputs
    test_folder: *const c_char, cases: *const [*const c_char], targets: *const [Target], mut tt: TestTable,
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

        for j in 0..targets.len() {
            let target = (*targets)[j];
            if let Some(row) = test_table_find_row(&mut tt, case_name, target) {
                match (*row).state {
                    TestState::Enabled => {
                        let outcome = execute_test(
                            // Inputs
                            test_folder, case_name, target,
                            // Outputs
                            cmd, sb,
                        )?;
                        match outcome {
                            Outcome::RunSuccess{stdout} =>
                                if strcmp((*row).expected_stdout, stdout) != 0 {
                                    fprintf(stderr(), c!("UNEXPECTED OUTCOME!!!\n"));
                                    jim_begin(jim);
                                    jim_string(jim, (*row).expected_stdout);
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
        Disable,
        Count,
    }
}

impl Action {
    unsafe fn name(self) -> *const c_char {
        match self {
            Self::Replay  => c!("replay"),
            Self::Record  => c!("record"),
            Self::Prune   => c!("prune"),
            Self::Disable => c!("disable"),
            Self::Count   => c!("count"),
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
    let comment              = flag_str(c!("comment"), ptr::null(), strdup(temp_sprintf(c!("Set the comment on disabled test cases when you do `-%s %s`"), flag_name(action_flag), Action::Disable.name()))); // TODO: memory leak

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
                    printf(c!("  %-*s - Replay the selected Test Matrix slice with expected outputs from %s.\n"), width, action.name(), json_path);
                }
                Action::Record => {
                    printf(c!("  %-*s - Record the selected Test Matrix slice into %s.\n"), width, action.name(), json_path);
                }
                Action::Prune  => {
                    printf(c!("  %-*s - Prune all the recordings from %s that are outside of the selected Test Matrix slice.\n"), width, action.name(), json_path);
                    printf(c!("  %-*s   Useful when you delete targets or test cases. Just delete a target or a test case and\n"), width, c!(""));
                    printf(c!("  %-*s   run `%s -%s %s` without any additional flags.\n"), width, c!(""), flag_program_name(), flag_name(action_flag), Action::Prune.name());
                }
                Action::Disable => {
                    printf(c!("  %-*s - Disable all the tests in the selected Test Matrix slice.\n"), width, action.name());
                    printf(c!("  %-*s   You can optionally set the comment with the -%s flag.\n"), width, c!(""), flag_name(comment));
                }
                Action::Count => {
                    printf(c!("  %-*s - Count the amount of rows in %s.\n"), width, action.name(), json_path);
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
            let mut tt = load_tt_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            record_tests(
                // Inputs
                *test_folder, da_slice(cases), da_slice(targets), &mut tt,
                // Outputs
                &mut cmd, &mut sb, &mut reports, &mut stats_by_target,
            )?;
            save_tt_to_json_file(json_path, tt, &mut jim)?;
        }
        Action::Replay => {
            let tt = load_tt_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            replay_tests(
                // Inputs
                *test_folder, da_slice(cases), da_slice(targets), tt,
                // Outputs
                &mut cmd, &mut sb, &mut reports, &mut stats_by_target, &mut jim,
            );
        }
        Action::Prune => {
            let tt = load_tt_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            save_tt_to_json_file(json_path, tt, &mut jim)?;
        }
        Action::Disable => {
            let mut case_width = 0;
            for i in 0..cases.count {
                let case_name = *cases.items.add(i);
                case_width = cmp::max(case_width, strlen(case_name));
            }

            let mut target_width = 0;
            for j in 0..targets.count {
                let target = *targets.items.add(j);
                target_width = cmp::max(target_width, strlen(target.name()));
            }

            let mut tt = load_tt_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            for i in 0..cases.count {
                let case_name = *cases.items.add(i);
                for j in 0..targets.count {
                    let target = *targets.items.add(j);
                    printf(c!("INFO: disabling %-*s for %-*s\n"), case_width, case_name, target_width, target.name());
                    if let Some(row) = test_table_find_row(&mut tt, case_name, target) {
                        (*row).state = TestState::Disabled;
                        if !(*comment).is_null() {
                            (*row).comment = *comment;
                        }
                    } else {
                        da_append(&mut tt, TestRow {
                            case_name,
                            target,
                            expected_stdout: c!(""),
                            state: TestState::Disabled,
                            comment: if (*comment).is_null() {
                                c!("")
                            } else {
                                *comment
                            },
                        });
                    }
                }
            }
            save_tt_to_json_file(json_path, tt, &mut jim)?;
        }
        Action::Count => {
            let tt = load_tt_from_json_file_if_exists(json_path, *test_folder, &mut sb, &mut jimp)?;
            printf(c!("%zu\n"), tt.count);
        }
    }

    Some(())
}

// TODO: generate HTML reports and deploy them somewhere automatically
// TODO: Make btest test historical mode
