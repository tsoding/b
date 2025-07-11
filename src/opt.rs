use crate::*;

pub unsafe fn optimize(c: *mut Compiler) {
    for i in 0..(*c).funcs.count {
        let func = (*c).funcs.items.add(i);
        merge_returns_pass(func);
    }
} 

pub unsafe fn merge_returns_pass(func: *mut Func) {
    for i in 0..(*func).body.count.saturating_add_signed(-1) {
        let op = (*func).body.items.add(i);
        let opcode = (*op).opcode;

        if opcode == Op::Return {
            (*op).opcode = Op::JmpLabel { label: (*func).label_count };
        }
    }

    if let Some(last) = da_last_mut(&mut (*func).body) {
        if (*last).opcode != Op::Return {
            fprintf(stderr(), c!("Incorrectly generated function %s: last op must be return"), (*func).name);
            abort();
        }
        (*last).opcode = Op::Label { label: (*func).label_count };
        da_append(&mut (*func).body, OpWithLocation { opcode: Op::Return, loc: (*last).loc });
    }
}
