CC=gcc
CFLAGS=-Wall -O3 --std=c99

BIN=recode
SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

all: $(BIN)

$(BIN): $(OBJ)
	$(CC) $^ -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(BIN) $(OBJ)
