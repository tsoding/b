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
    func['file'] = parse_string(f)
    func['params_count'] = parse_u64(f)
    func['auto_vars_count'] = parse_u64(f)
    func['ops'] = []
    body_len = parse_u64(f)
    for _ in range(body_len):

        op = {}

        # I really hope this goes via the same order
        op["loc"] = {
            "line_number": parse_u64(f),
            "line_offset": parse_u64(f),
        }

        opcode = parse_u8(f)

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

def parse_global(f):
    var = {}
    var['name'] = parse_string(f)
    
    value_count = parse_u64(f)
    var['values'] = []
    for _ in range(value_count):
        value = {}
        value["op"] = parse_u8(f)
        match value["op"]:
            case 0x00:
                value["name"] = parse_string(f)
            case 0x01:
                value["value"] = parse_u64(f)
            case 0x02:
                value["offset"] = parse_u64(f)

        var['values'].append(value)
    
    var["is_vec"] = bool(parse_u8(f))
    var["minimum_size"] = parse_u64(f)
    return var

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

    globals_size = parse_u64(f)
    bcode['globals'] = []
    for _ in range(globals_size):
        bcode['globals'].append(parse_global(f))
    
    funcs_len = parse_u64(f)
    bcode['functions'] = []
    for _ in range(funcs_len):
        bcode['functions'].append(parse_function(f))

    pprint.pp(bcode)

def dump_argument(arg: dict):
    match arg["type"]:
        case 0x00:
            return "BOGUS"
        case 0x01:
            return f"auto[{arg["index"]}]"
        case 0x02:
            return f"\*auto[{arg["index"]}]"
        case 0x03:
            return f"\&extrn[{arg["name"]}]"
        case 0x04:
            return f"\&auto[{arg["index"]}]"
        case 0x05:
            return arg["value"]
        case 0x06:
            return f"data[{arg["offset"]}]"

def p(s):
    print(s, end='')

binops = ["+",
          "-",
          "%",
          "/",
          "*",
          "<",
          ">",
          "==",
          "!=",
          ">=",
          "<=",
          "|",
          "&",
          ">>",
          "<<"]

for f in bcode["functions"]:
    print(f"{f["name"]}({f["params_count"]}, {f["auto_vars_count"]}) {'{'}")
    for op in f['ops']:
        print("    ", end='')
        match op["opcode"]:
            case 0x00:
                print("BOGUS")
            case 0x01:
                print(f"return({dump_argument(op["operand"])}" )
            case 0x05:
                print(f"auto[{op["index"]}] = -{dump_argument(op["operand"])}")
            case 0x07:
                print(
                    f"auto[{op["index"]}] = {dump_argument(op["lhs"])}",
                    binops[op["binop"]],
                    dump_argument(op["rhs"])
                ) 


    print('}')




