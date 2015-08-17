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


def main():
    parser = argparse.ArgumentParser(
        description='Recode .sid file for use in SID player.',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("SID", help="Original .sid file")
    parser.add_argument("--sid-file", default="gen_music.dat",
            help="Generated .sid file for player")
    parser.add_argument("--asm-file", default="gen_music.s",
            help="Generated .asm file with subroutines for setting shadow variables")
    parser.add_argument("--sid-address", default=1000,
            help="Base address of SID file in player (base 16)")
    parser.add_argument("--sa-routine-address",
            help="Base address of routines that set shadow variables (base 16)")
    parser.add_argument("--sa-label", default="SID_sh",
            help="Label that references shadow variables base address")

    args = parser.parse_args()

    with open(args.SID, 'rb') as f:
        data = f.read()

    # Convert base 16 addresses to base 10 numbers.
    args.sid_address = int(str(args.sid_address), 16)
    if args.sa_routine_address:
        args.sa_routine_address = int(str(args.sa_routine_address), 16)

    # If sa_routine_address was not supplied, derive it from SID base address
    # This means subroutines must be added *after* including SID file in the
    # player source code.
    # FIXME: This doesn't work yet. It must take into account dataOffset and
    # loadAddress from headers.
    if not args.sa_routine_address:
        args.sa_routine_address = args.sid_address + len(data)

    jsr_routines = []

    for m in sid_store_re.finditer(data):
        inst = instruction_from_match(m)

        try:
            idx = jsr_routines.index(inst)
        except ValueError:
            idx = len(jsr_routines)
            jsr_routines.append(inst)

        # Calculate subroutine address to jump to
        jsr_offset = idx * jsr_routine_size
        jsr_operand = args.sa_routine_address + jsr_offset

        print_found_match(m.start(), inst, jsr_operand)

        # Replace Store for JSR in data
        new_inst = "\x20" + pack_short(jsr_operand)
        data = data[:m.start()] + new_inst + data[m.end():]

    with open(args.sid_file, 'wb') as f:
        f.write(data)

    with open(args.asm_file, 'wb') as f:
        for opcode, operand in jsr_routines:
            a = stores[opcode] % "$%x" % operand
            b = stores[opcode] % "%s + $%x" % (args.sa_label, operand & 0xff)
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
