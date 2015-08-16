CC=cl65
CFLAGS=-u __EXEHDR__ -t c64 -C c64-asm.cfg

BIN=player.prg
SRC=main.s
SID=tune.sid
FIXED_SID=$(SID).1
GEN_ASM=gen_sid_write.s


all: $(BIN) recode

$(BIN): $(SRC) $(FIXED_SID) c64-asm.cfg
	$(CC) $(CFLAGS) -o $(BIN) $(SRC)

$(FIXED_SID): $(SID) recode
	recoder/recode $(SID) $(FIXED_SID) $(GEN_ASM)

recode: recoder/main.c
	make -C recoder


clean:
	rm -f $(BIN) $(FIXED_SID) $(GEN_ASM)
	make -C recoder clean

run: $(BIN)
	x64 $<


.PHONY: clean run
