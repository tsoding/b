use crate::nob::*;
use core::ffi::*;

pub unsafe fn run(cmd: *mut Cmd, emu: *const c_char, output_path: *const c_char, run_args: *const [*const c_char]) -> Option<()> {
    cmd_append! {cmd, emu, output_path,}
    da_append_many(cmd, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}
