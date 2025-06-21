use crate::nob::*;
use core::ffi::*;

unsafe fn prepare_cmd(cmd: *mut Cmd, emu: *const c_char, output_path: *const c_char, run_args: *const [*const c_char]) {
    cmd_append! {cmd, emu, output_path,}
    da_append_many(cmd, run_args);
}

pub unsafe fn run(cmd: *mut Cmd, emu: *const c_char, output_path: *const c_char, run_args: *const [*const c_char]) -> Option<()> {
    prepare_cmd(cmd, emu, output_path, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}

pub unsafe fn run_async(cmd: *mut Cmd, emu: *const c_char, output_path: *const c_char, run_args: *const [*const c_char]) -> Proc {
    prepare_cmd(cmd, emu, output_path, run_args);
    cmd_run_async_and_reset(cmd)
}
