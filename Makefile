CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm

all: whisper-v2

stage0: whisper-v1 $(SRC_FILES)
	./whisper-v1 whisper.scm -o stage0

stage1: stage0 core.h $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v2: stage1 core.h $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v2 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v2

clean:
	rm -f whisper-v1 bootstrap stage0 stage1

.PHONY: all clean
