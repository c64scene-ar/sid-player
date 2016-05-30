.PHONY: all clean

PRG=mp1.prg

all: $(PRG)

$(PRG): mp1.asm *.asm *.s *.bin
	acme $<

clean:
	rm -f labels.txt
