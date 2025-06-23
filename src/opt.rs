use crate::*;

pub unsafe fn optimize(c: *mut Compiler) {
	for i in 0..(*c).funcs.count {
		optimize_func(c,(*c).funcs.items.add(i));
    }
}

pub unsafe fn optimize_func(c: *mut Compiler,f:*mut Func){
	//function entry is special since no one is yet able to read/write
	//when we see the first label that means someone may actually jump from a new context
	//this means we can constant fold all autovar assigments here and its fine

	///right before a return we can dead code eliminate for similar reasons

	let start = *(*f).body.items;
	let _ = constant_fold_block(c,f,0);
}

pub unsafe fn constant_fold_block(c: *mut Compiler,f: *mut Func,mut block: usize) -> Option<usize> {	
	while block < (*f).body.count {
		let spot = (*f).body.items.add(block);

		(*spot).opcode = eval_constant_op((*spot).opcode,(*c).target);

		match (*spot).opcode {
			Op::Label{ label } => return Some(label),
			Op::Asm{..} | Op::Funcall{..} | Op::Return{..} => return None,
			Op::JmpIfNotLabel{..} => return None,
			Op::JmpLabel{label} => {
				//everything under this which is before the next label is unreachble
				block+=1;
				while block < (*f).body.count {
					
					let spot = (*f).body.items.add(block);
					match (*spot).opcode {
						Op::Label{..} | Op::Asm{..} => return None,
						_=>{}
					};

					// (*spot).opcode = Op::NoOp;
					block+=1;
				}

				return None;

			}
			_ => {}
		};

		block+=1;
	}

	None
}

pub unsafe fn negate_literal(lit: u64,target: Target) -> u64 {
    let word_size = target_word_size(target); // in bytes
    let bits = word_size * 8;
    let mask = if bits >= 64 { !0 } else { (1u64 << bits) - 1 };
    (!lit).wrapping_add(1) & mask
}


pub unsafe fn eval_constant_op(op:Op,target:Target) -> Op {
	 let mask = {
        let bits = target_word_size(target) * 8;
        if bits >= 64 { !0 } else { (1u64 << bits) - 1 }
    };

	match op {
		Op::UnaryNot { result, arg:Arg::Literal(lit)} => Op::AutoAssign{index:result,arg:Arg::Literal((lit==0) as u64)},
		Op::Negate { result, arg:Arg::Literal(lit)} => Op::AutoAssign{index:result,arg:Arg::Literal(negate_literal(lit,target))},
		Op::Binop { binop, index, lhs: Arg::Literal(a), rhs: Arg::Literal(b) } => {
            let val = match binop {
                Binop::Plus          => a.wrapping_add(b),
                Binop::Minus         => a.wrapping_sub(b),
                Binop::Mult          => a.wrapping_mul(b),
                Binop::Div           => if b == 0 { return op } else { a.wrapping_div(b) },
                Binop::Mod           => if b == 0 { return op } else { a.wrapping_rem(b) },

                Binop::Less          => (a < b) as u64,
                Binop::Greater       => (a > b) as u64,
                Binop::Equal         => (a == b) as u64,
                Binop::NotEqual      => (a != b) as u64,
                Binop::GreaterEqual  => (a >= b) as u64,
                Binop::LessEqual     => (a <= b) as u64,

                Binop::BitOr         => a | b,
                Binop::BitAnd        => a & b,
                Binop::BitShl        => a.wrapping_shl((b & 63) as u32),
                Binop::BitShr        => a.wrapping_shr((b & 63) as u32),
            } & mask;

            Op::AutoAssign { index, arg: Arg::Literal(val) }
        },
        Op::JmpIfNotLabel{label,arg:Arg::Literal(lit)} => {
        	if lit == 0 {
        		Op::JmpLabel{label}
        	}else{
        		Op::NoOp
        	}
        }
		_ => op

	}
}