.SILENT:
.PHONY: all test clean

ACME = acme
X64  = x64

PRG = mp_sw.prg
SRC = src/mp_sw.asm $(wildcard src/*.{asm,s}) $(wildcard src/*.bin)

all: $(PRG)

$(PRG): $(SRC)
	acme $<

test: $(PRG)
	x64 -pal $<

clean:
	rm -f labels.txt $(PRG)
