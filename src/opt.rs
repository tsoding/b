use crate::*;

pub unsafe fn optimize(c: *mut Compiler) {
	for i in 0..(*c).funcs.count {
		optimize_func(c,(*c).funcs.items.add(i));
    }
}

#[derive(Clone, Copy)]
pub struct BlockStyle{
	no_entries:bool,
	no_readers:bool,
}

pub unsafe fn optimize_func(c: *mut Compiler,f:*mut Func){
	//function entry is special since no one is yet able to read/write
	//when we see the first label that means someone may actually jump from a new context
	//this means we can constant fold all autovar assigments here and its fine

	let mut style = BlockStyle{no_readers:false,no_entries:true};
	let mut block = 0;

	//rest is normal
	while block < (*f).body.count {
		block=optimize_block(c,f,block,&mut style);
	}

}

pub unsafe fn eliminate_block_end(c: *mut Compiler,f: *mut Func, block: usize,style:*mut BlockStyle) -> usize {
	let mut id = block;
	
	while id < (*f).body.count {

		let spot = (*f).body.items.add(id);
		match (*spot).opcode {
			Op::Label{..} | Op::Asm{..}  => {
				//the asm may have a label and then fall through

				(*style).no_readers = false;
				(*style).no_entries = false;
				return id
			},
			_=>{(*spot).opcode = Op::NoOp;}
		};

		id+=1;
	}

	id
}

pub unsafe fn optimize_block(c: *mut Compiler,f: *mut Func, block: usize,style:*mut BlockStyle) -> usize {	
	let mut id = block;
	
	while id < (*f).body.count {
		let spot = (*f).body.items.add(id);

		(*spot).opcode = eval_constant_op((*spot).opcode,(*c).target,*style);

		match (*spot).opcode {
			Op::Label{ .. } | Op::Asm{..} => {
				(*style).no_entries = false;
				(*style).no_readers = false;
				return id+1
			},
			Op::Funcall{..}  => {
				(*style).no_readers = false;
				return id+1
			},
			Op::Return{..} => {
				//since this is returning out no one can possibly read the autovars
				//because autovars are function local
				//in fact this is an even more agressive type of constant folding we could do here
				if (*style).no_readers == false {
					(*style).no_readers = true;
					return optimize_block(c,f,block,style);
				}
				return eliminate_block_end(c,f,id+1,style);

			},
			Op::JmpLabel{..} => {
				return eliminate_block_end(c,f,id+1,style);

			}
			_ => {}
		};

		id+=1;
	}

	id
}


pub unsafe fn eval_constant_op(op:Op,target:Target,style:BlockStyle) -> Op{
	if style.no_readers {
		eval_constant_op_strong(op,target)
	}else {
		eval_constant_op_weak(op,target)
	}
}

pub unsafe fn eval_constant_op_strong(op:Op,target:Target) -> Op {
	eval_constant_op_weak(op,target)
}

pub unsafe fn eval_constant_op_weak(op:Op,target:Target) -> Op {
	 let mask = {
        let bits = target_word_size(target) * 8;
        if bits >= 64 { !0 } else { (1u64 << bits) - 1 }
    };

	match op {
		Op::UnaryNot { result, arg:Arg::Literal(lit)} => Op::AutoAssign{index:result,arg:Arg::Literal((lit==0) as u64)},
		Op::Negate { result, arg:Arg::Literal(lit)} => Op::AutoAssign{index:result,arg:Arg::Literal((!lit).wrapping_add(1) & mask)},
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