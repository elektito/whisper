CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm

all: whisper-v1

bootstrap: bootstrap.c
	gcc $(CFLAGS) bootstrap.c -o bootstrap

stage0: bootstrap $(SRC_FILES)
	./bootstrap whisper.scm -o stage0

stage1: stage0 $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f -Wl,-s

whisper-v1: stage1 $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v1 -f -Wl,-s
	diff stage1 whisper-v1

clean:
	rm -f whisper-v1 bootstrap stage0 stage1

.PHONY: all clean
