use crate::nob::*;
use core::ffi::*;

unsafe fn prepare_cmd(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) {
    // TODO: document that you may need wine as a system package to cross-run fasm-x86_64-windows
    if !cfg!(target_os = "windows") {
        cmd_append! {
            cmd,
            c!("wine"),
        }
    }

    cmd_append! {cmd, output_path,}
    da_append_many(cmd, run_args);
}

pub unsafe fn run(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) -> Option<()> {
    prepare_cmd(cmd, output_path, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}

pub unsafe fn run_async(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) -> Proc {
    prepare_cmd(cmd, output_path, run_args);
    cmd_run_async_and_reset(cmd)
}
