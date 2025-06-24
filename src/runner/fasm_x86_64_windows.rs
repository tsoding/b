use crate::nob::*;
use crate::crust::libc::*;
use core::ffi::*;

pub unsafe fn run(cmd: *mut Cmd, output_path: *const c_char, run_args: *const [*const c_char]) -> Option<()>{
    let needs_wine = !cfg!(target_os = "windows");
    
    if needs_wine {
        cmd_append! {
            cmd,
            c!("wine"),
        }
    }

    cmd_append! {cmd, output_path,}
    da_append_many(cmd, run_args);

    if !cmd_run_sync_and_reset(cmd) { 
        if needs_wine {
            // Check if wine is installed
            cmd_append! { cmd, c!("which"), c!("wine"), }
            let wine_not_installed = cmd_run_sync_and_reset(cmd);
            
            if wine_not_installed {
                fprintf(stderr(), c!("ERROR: Failed to run Windows executable. Wine does not appear to be installed.\n"));
                fprintf(stderr(), c!("  - Ubuntu/Debian: sudo apt install wine\n"));
            } else {
                fprintf(stderr(), c!("ERROR: Failed to run Windows executable.\n"));
            }
        }
        return None; 
    }
    Some(())
}
