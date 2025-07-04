use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;
use core::mem::zeroed;

pub unsafe fn run(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char], stdout_path: Option<*const c_char>) -> Option<()> {
    if !cfg!(target_arch = "aarch64") {
        fprintf(stderr(), c!("This runner is only for aarch64 Darwin, but the current target is not aarch64 Darwin."));
        return None;
    }

    // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Darwin. It has to be `./program`.
    let run_path: *const c_char;
    if (strchr(output_path, '/' as c_int)).is_null() {
        run_path = temp_sprintf(c!("./%s"), output_path);
    } else {
        run_path = output_path;
    }

    cmd_append! {
        cmd,
        run_path,
    }

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
