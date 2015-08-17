CC=cl65
CFLAGS=-u __EXEHDR__ -t c64 -C c64-asm.cfg -l listing

BIN=player.prg
SRC=main.s
SID=music.sid
GEN_SID=gen_music.dat
GEN_ASM=gen_music.inc

all: $(BIN)

$(BIN): $(SRC) $(GEN_SID) c64-asm.cfg
	$(CC) $(CFLAGS) -o $(BIN) $(SRC)

$(GEN_SID): $(SID) recode_sid.py
	./recode_sid.py $(SID) --output-sid $(GEN_SID) --output-asm $(GEN_ASM)

clean:
	rm -f $(BIN) $(GEN_SID) $(GEN_ASM) *.o

run: $(BIN)
	x64 $<

.PHONY: clean run
