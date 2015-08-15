# player
BIN=player.prg
SRC=main.s
SID=tune.sid
FIXED_SID=$(SID).fixed

# recoder
CC=gcc
CFLAGS=-Wall -O3 --std=c99


all: $(BIN) recoder

$(BIN): $(SRC) $(FIXED_SID)
	cl65 -o $(BIN) -u __EXEHDR__ -t c64 -C c64-asm.cfg $(SRC)

$(FIXED_SID): $(SID) recoder
	./recoder $(SID) $(FIXED_SID)

recoder: recoder.c
	$(CC) $(CFLAGS) $^ -o $@


clean:
	rm -f $(BIN) recoder

run: $(BIN)
	x64 $<


.PHONY: clean run
