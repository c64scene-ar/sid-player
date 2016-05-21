.PHONY: all clean

PRG=mp_sw.prg

all: $(PRG)

$(PRG): mp_sw.asm *.asm *.bin
	acme $<

test: $(PRG)
	x64 -pal $<

clean:
	rm -f labels.txt $(PRG)
