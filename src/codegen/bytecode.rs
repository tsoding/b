// NAME     OPCODE   ARGUMENT       NOTES
// BOGUS    00
// RET      01       0
// STORE    02       3,0
// ExrnAss  03       1,2,0
// AutoAss  04       3,0
// Negate   05       3,0
// UnaryNot 06       3,0
// Binop    07       3,4,0,0
// Asm      08       1[(1,2)]       Array of Strings
// Label    09       3
// Jmp      0A       3              Op::JmpLabel
// JmpNot   0B       3              Op::JmpIfNotLabel
// Funcall  0C       3,0,1[0]       Array of arguments

// Argument Table:
// ID | Name     | Type
// =====================
// 0  | argument | Argument
// 1  | size     | u64
// 2  | string   | i8[size]
// 3  | index    | u64
// 4  | Op       | u8

//
// Extra Types:
//      Argument:
//          type: u8
//          argument:
//          0: Bogus 
//          1: Autovar(u64)
//          2: Deref(u64)
//          3: RefExtrn{size: u64, name: i8[size]}
//          4: RefAutoVar(u64)
//          5: Literal(u64)
//          6: DataOffset(u64+data_offset) 
//          7: Extrn{size: u64, name: i8[size]}

//      Binop:
//              0:  Plus,
//              1:  Minus,
//              2:  Mult,
//              3:  Mod,
//              4:  Div,
//              5:  Less,
//              6:  Greater,
//              7:  Equal,
//              8:  NotEqual,
//              9:  GreaterEqual
//              10: LessEqual,
//              11: BitOr,
//              12: BitAnd,
//              13: BitShl,
//              14: BitShr

use core::ffi::*;
use crate::nob::*;
use crate::{Op, Binop, OpWithLocation, Arg, Func, Global, ImmediateValue, Compiler};
use crate::crust::libc::*;

pub unsafe fn dump_arg_call(arg: Arg, output: *mut Array<u8>) {
    match arg {
        Arg::RefExternal(name) | Arg::External(name) => {
            push_opcode(output, 0x00);
            append_string(output, name);
        }
        arg => {
            push_opcode(output, 0x01);
            dump_arg(output, arg);
        }
    };
}

pub unsafe fn push_opcode(output: *mut Array<u8>, op: usize) {
    append_u8(output, op.try_into().unwrap());
}

pub unsafe fn dump_arg(output: *mut Array<u8>, arg: Arg) {
    match arg {
        Arg::External(name)     => {
            push_opcode(output, 0x07);
            append_string(output, name);
        }
        Arg::Deref(index)       => {
            push_opcode(output, 0x02);
            append_u64(output, index.try_into().unwrap());
        }
        Arg::RefAutoVar(index)  => {
            push_opcode(output, 0x04);
            append_u64(output, index.try_into().unwrap());
        }
        Arg::RefExternal(name)  => {
            push_opcode(output, 0x3);
            append_string(output, name);
        }
        Arg::Literal(value)     => {
            push_opcode(output, 0x05);
            append_u64(output, value.try_into().unwrap());
        }
        Arg::AutoVar(index)     => {
            push_opcode(output, 0x01);
            append_u64(output, index.try_into().unwrap());
        }
        Arg::DataOffset(offset) => {
            push_opcode(output, 0x06);
            printf(c!("%i\n"), offset);
            append_u64(output, offset.try_into().unwrap());
        }
        Arg::Bogus              => {
            push_opcode(output, 0x00);
        } 
    };
}

pub unsafe fn append_u8(output: *mut Array<u8>, content: u8) {
        da_append(output, content);
}

pub unsafe fn append_u64(output: *mut Array<u8>, content: u64) {
    let mut data = content;
    for _ in 0..8 {
        append_u8(output, (data & 0xFF).try_into().unwrap());
        data <<= 8;
    }
}

pub unsafe fn append_string(output: *mut Array<u8>, content: *const c_char) {
    let len: usize = strlen(content);
    append_u64(output, len.try_into().unwrap());
    sb_appendf(output as *mut String_Builder, c!("%s"), content );
}

pub unsafe fn generate_function(name: *const c_char, params_count: usize, auto_vars_count: usize, body: *const [OpWithLocation], output: *mut Array<u8>) {
    append_string(output, name);
    append_string(output, (*body)[0].loc.input_path);
    append_u64(output, params_count.try_into().unwrap());
    append_u64(output, auto_vars_count.try_into().unwrap());
    append_u64(output, body.len().try_into().unwrap());
    for i in 0..body.len() {
        let op = (*body)[i];
        append_u64(output, op.loc.line_number.try_into().unwrap());
        append_u64(output, op.loc.line_offset.try_into().unwrap());
        match op.opcode {
            Op::Bogus => push_opcode(output, 0x00),
            Op::Return {arg} => {
                push_opcode(output, 0x01);
                
                if let Some(arg) = arg {
                    dump_arg(output, arg);
                } else {
                    push_opcode(output, 0x00);
                }
            },
            Op::Store{index, arg} => {
                push_opcode(output, 0x02);
                append_u64(output, index.try_into().unwrap());
                dump_arg(output, arg);
            }
            Op::ExternalAssign{name, arg} => {
                push_opcode(output, 0x03);
                append_string(output, name);
                dump_arg(output, arg);
            }
            Op::AutoAssign{index, arg} => {
                push_opcode(output, 0x04);
                append_u64(output, index.try_into().unwrap());
                dump_arg(output, arg);
            }
            Op::Negate{result, arg} => {
                push_opcode(output, 0x05);
                append_u64(output, result.try_into().unwrap());
                dump_arg(output, arg);
            }
            Op::UnaryNot{result, arg} => {
                push_opcode(output, 0x06);
                append_u64(output, result.try_into().unwrap());
                dump_arg(output, arg);
            }
            Op::Binop {binop, index, lhs, rhs} => {
                push_opcode(output, 0x07);
                append_u64(output, index.try_into().unwrap());
                match binop {
                    Binop::BitOr        => push_opcode(output, 0x0B),
                    Binop::BitAnd       => push_opcode(output, 0x0C), 
                    Binop::BitShl       => push_opcode(output, 0x0D),
                    Binop::BitShr       => push_opcode(output, 0x0E),
                    Binop::Plus         => push_opcode(output, 0x00), 
                    Binop::Minus        => push_opcode(output, 0x01), 
                    Binop::Mod          => push_opcode(output, 0x02),
                    Binop::Div          => push_opcode(output, 0x03), 
                    Binop::Mult         => push_opcode(output, 0x04), 
                    Binop::Less         => push_opcode(output, 0x05), 
                    Binop::Greater      => push_opcode(output, 0x06), 
                    Binop::Equal        => push_opcode(output, 0x07),
                    Binop::NotEqual     => push_opcode(output, 0x08),
                    Binop::GreaterEqual => push_opcode(output, 0x09),
                    Binop::LessEqual    => push_opcode(output, 0x0A), 
                };
                dump_arg(output, lhs);
                dump_arg(output, rhs);
            }
            Op::Funcall{result, fun, args} => {
                push_opcode(output, 0x0C);
                append_u64(output, result.try_into().unwrap());
                dump_arg_call(fun, output);
                append_u64(output, args.count.try_into().unwrap());
                for i in 0..args.count {
                    dump_arg(output, *args.items.add(i));
                }
            }
            Op::Asm {stmts} => {
                push_opcode(output, 0x08);
                append_u64(output, stmts.count.try_into().unwrap());
                for i in 0..stmts.count {
                    let arg = *stmts.items.add(i);
                    append_string(output, arg.line);
                }
            }

            Op::Label {label} => {
                push_opcode(output, 0x09);
                append_u64(output, label.try_into().unwrap());
            }
            Op::JmpLabel {label} => {
                push_opcode(output, 0x0A);
                append_u64(output, label.try_into().unwrap());
            }
            Op::JmpIfNotLabel {label, arg} => {
                push_opcode(output, 0x0B);
                append_u64(output, label.try_into().unwrap());
                dump_arg(output, arg);
            }
        }
    }
}

pub unsafe fn generate_funcs(output: *mut Array<u8>, funcs: *const [Func]) {
    append_u64(output, funcs.len().try_into().unwrap());
    for i in 0..funcs.len() {
        generate_function((*funcs)[i].name, (*funcs)[i].params_count, (*funcs)[i].auto_vars_count, da_slice((*funcs)[i].body), output);
    }
}

pub unsafe fn generate_extrns(output: *mut Array<u8>, extrns: *const [*const c_char]) {
    append_u64(output, extrns.len().try_into().unwrap());
    for i in 0..extrns.len() {
        append_string(output, (*extrns)[i]);
    }
}

pub unsafe fn generate_globals(output: *mut Array<u8>, globals: *const [Global]) {
    for i in 0..globals.len() {
        todo!();
    }
}

//data is:
//     u8[data.len()]
pub unsafe fn generate_data_section(output: *mut Array<u8>, data: *const [u8]) {
    append_u64(output, (*data).len().try_into().unwrap());
    if data.len() > 0 {
        for i in (0..(*data).len()) {
            append_u8(output, (*data)[i]);
        }
    }
}

const version: u8 = 0x00;


// Get the last bytes of the pgram for the table
pub unsafe fn generate_program(output: *mut Array<u8>, c: *const Compiler) {
    append_u8(output, 0xDE);
    append_u8(output, 0xBC);

    //VERSION
    da_append(output, version);
    let extrn_pos = (*output).count;
    generate_extrns(output, da_slice((*c).extrns));
    let data_pos = (*output).count;
    generate_data_section(output, da_slice((*c).data));
    let globals_pos = (*output).count;
    generate_globals(output, da_slice((*c).globals));
    let funcs_pos = (*output).count;
    generate_funcs(output, da_slice((*c).funcs));

    append_u64(output, extrn_pos.try_into().unwrap());
    append_u64(output, data_pos.try_into().unwrap());
    append_u64(output, globals_pos.try_into().unwrap());
    append_u64(output, funcs_pos.try_into().unwrap());
}
