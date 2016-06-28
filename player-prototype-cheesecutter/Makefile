.SILENT:
.PHONY: all test clean

ACME = acme
X64  = x64

PRG = mp1.prg
SRC = src/mp1.asm $(wildcard src/*.{asm,s})

all: $(PRG)

$(PRG): $(SRC)
	acme $<

test: $(PRG)
	x64 $<

clean:
	rm -f labels.txt $(PRG)
