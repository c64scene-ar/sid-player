#!/usr/bin/env python
# encoding: utf-8

from sys import exit, argv
import re
import struct

stores = {
    0x8d: "sta %s",
    0x8e: "stx %s",
    0x8c: "sty %s",
    0x9d: "sta %s, x",
    0x99: "sta %s, y",
}

sid_store_re = re.compile(r"""
    (?P<opcode>[\x8d\x9d\x99\x8e\x8c])
    (?P<operand>[\x00-\x20]\xd4)
""", re.X)

jsr_opcode = 0x20
jsr_routine_asm = """
    %s
    %s
    rts
    nop
"""
jsr_routine_size = 8        # Routine size in bytes

# TODO: This should be command-lne options
jsr_base_addr = 0xc500
sidregs_base_addr = 'SID_regs_base'


def main():
    # TODO: Use optparse for command-line options
    if len(argv) != 4:
        print("Usage: %s FILE NEW_FILE GEN_ASM_FILE" % argv[0])
        return 1

    _, src, dst, gen_asm = argv

    jsr_routines = []

    with open(src, 'rb') as f:
        data = f.read()

    for m in sid_store_re.finditer(data):
        inst = instruction_from_match(m)

        try:
            idx = jsr_routines.index(inst)
        except ValueError:
            idx = len(jsr_routines)
            jsr_routines.append(inst)

        jsr_offset = idx * jsr_routine_size
        jsr_operand = jsr_base_addr + jsr_offset
        new_inst = "\x20" + struct.pack("<H", jsr_operand)

        print_found_match(m.start(), inst, jsr_operand)

        data = data[:m.start()] + new_inst + data[m.end():]

    with open(dst, 'wb') as f:
        f.write(data)

    with open(gen_asm, 'wb') as f:
        for inst in jsr_routines:
            a = stores[inst['opcode']] % "$%x" % inst['operand']
            b = stores[inst['opcode']] % "%s + $%x" % (sidregs_base_addr, inst['operand'] & 0xff)
            f.write(jsr_routine_asm % (a, b))

    return 0

def instruction_from_match(m):
    h = m.groupdict()
    h['opcode'] = ord(h['opcode'])
    h['operand'] = unpack_short(h['operand'])
    return h

def print_found_match(pos, inst, jsr_operand):
    inst_str = stores[inst['opcode']] % "$%x" % inst['operand']
    print("{0:04x}\t{1}\t-> jsr ${2:x}".format(pos, inst_str, jsr_operand))

def unpack_short(s):
    return struct.unpack("<H", s)[0]


if __name__ == "__main__":
    exit(main())
