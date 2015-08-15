XA=xa
BIN=simple_player.prg

CFLAGS=-Wall -O3 --std=c99

all: $(BIN) recoder

$(BIN): main.s $(SRC)
	$(XA) -OPETSCII -o $@ $^

recoder: recoder.c
	$(CC) $(CFLAGS) $^ -o $@

clean:
	rm -f $(BIN) recoder

run: $(BIN)
	x64 $<

.PHONY: clean run
