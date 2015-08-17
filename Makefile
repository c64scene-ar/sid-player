CC=cl65
CFLAGS=-u __EXEHDR__ -t c64 -C c64-asm.cfg

BIN=player.prg
SRC=main.s
SID=music.sid
FIXED_SID=music.dat
GEN_ASM=gen_sid_write.inc


all: $(BIN)

$(BIN): $(SRC) $(FIXED_SID) c64-asm.cfg
	$(CC) $(CFLAGS) -o $(BIN) $(SRC)

$(FIXED_SID): $(SID) recode_sid.py
	./recode_sid.py $(SID) $(FIXED_SID) $(GEN_ASM)


clean:
	rm -f $(BIN) $(FIXED_SID) $(GEN_ASM) *.o

run: $(BIN)
	x64 $<


.PHONY: clean run
