XA=xa
XAFLAGS=-OPETSCII -llabels
CFLAGS=-Wall -O3 --std=c99

BIN=player.prg
SRC=main.s
SID=tune.sid
FIXED_SID=$(SID).fixed

all: $(BIN) recoder

$(BIN): $(SRC) $(FIXED_SID)
	$(XA) $(XAFLAGS) -o $@ $(SRC)

$(FIXED_SID): $(SID) recoder
	./recoder $(SID) $(FIXED_SID)

recoder: recoder.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f $(BIN) recoder

run: $(BIN)
	x64 $<

.PHONY: clean run
