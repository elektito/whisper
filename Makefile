CFLAGS ?=

all: whisper

whisper: whisper.c
	gcc $(CFLAGS) whisper.c -o whisper

clean:
	rm -f whisper

.PHONY: all clean
