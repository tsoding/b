import math
import pprint
import subprocess
import sys

def parse_u8(f):
    return int.from_bytes(f.read(1), "little", signed=False) 

def parse_u64(f):
    return int.from_bytes(f.read(8), "little", signed=False)

def parse_string(f) -> str:
    size = parse_u64(f);
    chars = []
    for _ in range(size):
        chars.append(chr(parse_u8(f)))
    

    return "".join(chars)

def parse_u16(f) -> int:
    return int.from_bytes(f.read(2), "little", signed=False)


def parse_argument(f):
    argument = {}
    argument["type"] = parse_u8(f)
    match argument["type"]:
        case 0x00:
            pass
        case 0x01 | 0x02 | 0x04:
            argument["index"] = parse_u64(f)
        case 0x03 | 0x07:
            argument["name"] = parse_string(f)
        case 0x05:
            argument["value"] = parse_u64(f)
        case 0x06:
            argument["offset"] = parse_u64(f)
        case _:
            assert False, f"Unknown argument type 0x{argument['type']:02x}"
    return argument



def parse_function(f):
    func = {}
    func['name'] = parse_string(f)
    func['params_count'] = parse_u64(f)
    func['auto_vars_count'] = parse_u64(f)
    func['ops'] = []
    body_len = parse_u64(f)
    for _ in range(body_len):
        opcode = parse_u8(f)

        op = {}
        op['opcode'] = opcode
        match opcode:
            case 0x01:
                op["operand"] = parse_argument(f)
            case 0x05:
                op["index"]   = parse_u64(f)
                op["operand"] = parse_argument(f)
            case 0x07:
                op["index"] = parse_u64(f)
                op["binop"] = parse_u8(f)
                op["lhs"]   = parse_argument(f)
                op["rhs"]   = parse_argument(f)
            case 0x09:
                op['index'] = parse_u64(f)
            case 0x0A:
                op['index'] = parse_u64(f)
            case 0x0B:
                op['index'] = parse_u64(f)
                op['arg'] = parse_argument(f)

            case 0x0C:
                op['index'] = parse_u64(f)
                op['function_type'] = parse_u8(f)
                if op['function_type'] == 0x00:
                    op['function'] = parse_string(f)
                else:
                    op['function'] = parse_argument(f)
                argument_count = parse_u64(f)
                op['arguments'] = []
                for _ in range(argument_count):
                    op['arguments'].append(parse_argument(f))
            
            case _:
                assert False, f"Opcode 0x{opcode:02x} not recognised"
        func['ops'].append(op)
    return func

subprocess.run("make")
subprocess.run(["./build/b", "-t", "bytecode", "-o", "./build/bytecode.ir", sys.argv[1]])

with open("./build/bytecode.ir", 'rb') as f:
    bcode = {}
    magic = [parse_u8(f), parse_u8(f)]
    bcode['magic'] = f'{magic[1]:02x}{magic[0]:02x}'
    assert bcode['magic'] == 'bcde'

    bcode['version'] = parse_u8(f)
    assert bcode['version'] == 0

    extrn_size = parse_u64(f)
    bcode['extrns'] = []
    for _ in range(extrn_size):
        bcode['extrns'].append(parse_string(f))

    data_size = parse_u64(f)
    bcode['data'] = []
    for _ in range(data_size):
        bcode['data'].append(chr(parse_u8(f)))

    bcode['data'] = ''.join(bcode['data']).split("\x00")
    funcs_len = parse_u64(f)
    bcode['functions'] = []
    for _ in range(funcs_len):
        bcode['functions'].append(parse_function(f))

    pprint.pp(bcode)
    

