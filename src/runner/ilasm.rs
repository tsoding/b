use core::ffi::*;
use core::mem::zeroed;
use crate::nob::*;

pub unsafe fn run(cmd: *mut Cmd, program_path: *const c_char, run_args: *const [*const c_char], stdout_path: Option<*const c_char>, mono: bool) -> Option<()> {
    if mono && !cfg!(target_os = "windows") {
        cmd_append! {
            cmd,
            c!("mono"),
        }
    }

    if !mono {
        cmd_append! {
            cmd,
            c!("dotnet"),
        }
    }

    cmd_append!{ cmd, program_path, }

    da_append_many(cmd, run_args);

    if let Some(stdout_path) = stdout_path {
        let mut fdout = fd_open_for_write(stdout_path);
        let mut redirect: Cmd_Redirect = zeroed();
        redirect.fdout = &mut fdout;
        if !cmd_run_sync_redirect_and_reset(cmd, redirect) { return None; }
    } else {
        if !cmd_run_sync_and_reset(cmd) { return None; }
    }
    Some(())
}
