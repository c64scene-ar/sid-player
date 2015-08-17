#!/usr/bin/env python
# encoding: utf-8

from sys import exit, argv
import re
import struct
import argparse

stores = {
    0x8d: "sta %s",
    0x8e: "stx %s",
    0x8c: "sty %s",
    0x9d: "sta %s, x",
    0x99: "sta %s, y",
}

sid_store_re = re.compile(r"""
    (?P<opcode>[\x8d\x9d\x99\x8e\x8c])
    (?P<operand>[\x00-\x18]\xd4)
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
    parser = argparse.ArgumentParser(description='Recode .sid file for use in SID player.')
    parser.add_argument("sid", help="Original .sid file")
    parser.add_argument("gen_sid", help="Generated .sid file for player")
    parser.add_argument("gen_asm", help="Generated .asm file with subroutines for setting shadow variables")

    args = parser.parse_args()

    jsr_routines = []

    with open(args.sid, 'rb') as f:
        data = f.read()

    for m in sid_store_re.finditer(data):
        inst = instruction_from_match(m)

        try:
            idx = jsr_routines.index(inst)
        except ValueError:
            idx = len(jsr_routines)
            jsr_routines.append(inst)

        # Calculate subroutine address to jump to
        jsr_offset = idx * jsr_routine_size
        jsr_operand = jsr_base_addr + jsr_offset

        print_found_match(m.start(), inst, jsr_operand)

        # Replace Store for JSR in data
        new_inst = "\x20" + pack_short(jsr_operand)
        data = data[:m.start()] + new_inst + data[m.end():]

    with open(args.gen_sid, 'wb') as f:
        f.write(data)

    with open(args.gen_asm, 'wb') as f:
        for opcode, operand in jsr_routines:
            a = stores[opcode] % "$%x" % operand
            b = stores[opcode] % "%s + $%x" % (sidregs_base_addr, operand & 0xff)
            f.write(jsr_routine_asm % (a, b))

    return 0

def instruction_from_match(m):
    opcode, operand = m.groups()
    opcode = ord(opcode)
    operand = unpack_short(operand)
    return (opcode, operand)

def print_found_match(pos, inst, jsr_operand):
    opcode, operand = inst
    inst_str = stores[opcode] % "$%x" % operand
    print("{0:04x}\t{1}\t-> jsr ${2:x}".format(pos, inst_str, jsr_operand))

def unpack_short(s):
    return struct.unpack("<H", s)[0]

def pack_short(n):
    return struct.pack("<H", n)


if __name__ == "__main__":
    exit(main())
