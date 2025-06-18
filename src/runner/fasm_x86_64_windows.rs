use crate::nob::*;
use core::ffi::*;

pub unsafe fn run(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) -> Option<()>{
    // TODO: document that you may need wine as a system package to cross-run fasm-x86_64-windows
    if !cfg!(target_os = "windows") {
        cmd_append! {
            cmd,
            c!("wine"),
        }
    }

    cmd_append! {cmd, output_path,}
    da_append_many(cmd, run_args);

    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}
