# player
BIN=player.prg
SRC=main.s
SID=tune.sid
FIXED_SID=$(SID).1
GEN_ASM=gen_sid_write.s

# recoder
CC=gcc
CFLAGS=-Wall -O3 --std=c99


all: $(BIN) recoder

$(BIN): $(SRC) $(FIXED_SID) c64-asm.cfg
	cl65 -o $(BIN) -u __EXEHDR__ -t c64 -C c64-asm.cfg $(SRC)

$(FIXED_SID): $(SID) recoder
	./recoder $(SID) $(FIXED_SID) $(GEN_ASM)

recoder: recoder.c
	$(CC) $(CFLAGS) $^ -o $@


clean:
	rm -f $(BIN) recoder $(FIXED_SID) $(GEN_ASM)

run: $(BIN)
	x64 $<


.PHONY: clean run
