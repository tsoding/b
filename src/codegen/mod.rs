pub mod gas_aarch64;
pub mod gas_x86_64;
pub mod mos6502;
pub mod uxn;
pub mod ilasm_mono;

use core::ffi::*;
use core::ptr;
use crate::crust::libc::*;
use crate::nob::*;

pub const CODEGEN_FLAG_NAME: *const c_char = c!("C");

#[derive(Clone, Copy)]
pub enum ParamValue {
    Flag   { var: *mut bool                                  },
    String { var: *mut *const c_char, default: *const c_char },
    Hex    { var: *mut u64,           default: u64           },
}

#[derive(Clone, Copy)]
pub struct Param {
    pub name:        *const c_char,
    pub description: *const c_char,
    pub value:       ParamValue,
}

pub unsafe fn parse_args(params: *const [Param], args: *const[*const c_char]) -> Result<(), *const c_char> {
    for j in 0..params.len() {
        let param = (*params)[j];
        match param.value {
            ParamValue::Flag   {var}          => *var = false,
            ParamValue::String {var, default} => *var = default,
            ParamValue::Hex    {var, default} => *var = default,
        }
    }

    'args: for i in 0..args.len() {
        let arg = (*args)[i];
        let equal = strchr(arg, '=' as i32);
        let (key, value) = if equal.is_null() {
            (arg, ptr::null())
        } else {
            (temp_sprintf(c!("%.*s"), equal.offset_from(arg), arg) as *const c_char, equal.wrapping_add(1))
        };
        for j in 0..params.len() {
            let param = (*params)[j];
            if strcmp(param.name, key) == 0 {
                match param.value {
                    ParamValue::Flag{var, ..} => {
                        if !value.is_null() {
                            return Err(temp_sprintf(c!("%s is a flag and may not have a value"), key));
                        }
                        *var = true;
                    }
                    ParamValue::String{var, ..} => {
                        *var = value;
                    }
                    ParamValue::Hex{var, ..} => {
                        if value.is_null() {
                            return Err(temp_sprintf(c!("%s expects a hexadecimal value"), key));
                        }
                        let mut endptr: *mut c_char = ptr::null_mut();
                        *var = strtoull(value, &mut endptr, 16);
                        if endptr as *const c_char == value || *endptr != 0 {
                            return Err(temp_sprintf(c!("%s expects a hexadecimal value"), key));
                        }
                    }
                }
                continue 'args
            }
        }
        return Err(temp_sprintf(c!("Unknown codegen parameter %s"), key));
    }
    Ok(())
}

pub unsafe fn print_params_help(params: *const [Param]) {
    for i in 0..params.len() {
        let param = (*params)[i];
        match param.value {
            ParamValue::Flag{..} => {
                fprintf(stderr(), c!("    -%s %s\n"), CODEGEN_FLAG_NAME, param.name);
                fprintf(stderr(), c!("        %s\n"), param.description);
            }
            ParamValue::String{default, ..} => {
                fprintf(stderr(), c!("    -%s %s=\"<str>\"\n"), CODEGEN_FLAG_NAME, param.name);
                fprintf(stderr(), c!("        %s\n"), param.description);
                if !default.is_null() && *default != 0 {
                    fprintf(stderr(), c!("        Default: %s\n"), default);
                }
            },
            ParamValue::Hex{default, ..} => {
                fprintf(stderr(), c!("    -%s %s=<hex>\n"), CODEGEN_FLAG_NAME, param.name);
                fprintf(stderr(), c!("        %s\n"), param.description);
                fprintf(stderr(), c!("        Default: %lX\n"), default);
            }
        };
    }
}
