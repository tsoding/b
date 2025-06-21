use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;

unsafe fn prepare_cmd(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) {
    // if the user does `b program.b -run` the compiler tries to run `program` which is not possible on Linux. It has to be `./program`.
    let run_path: *const c_char;
    if (strchr(output_path, '/' as c_int)).is_null() {
        run_path = temp_sprintf(c!("./%s"), output_path);
    } else {
        run_path = output_path;
    }

    cmd_append! {cmd, run_path,}
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
