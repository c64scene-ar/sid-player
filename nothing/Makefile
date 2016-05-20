.PHONY: all clean

PRG=mp1.prg

all: $(PRG)

$(PRG): mp1.asm *.asm *.s *.bin
	acme $<

test: $(PRG)
	x64 $<

clean:
	rm -f labels.txt $(PRG)
