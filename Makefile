CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm macro.scm

all: whisper-v4

stage0: $(SRC_FILES)
	./whisper-v3 whisper.scm -o stage0

stage1: stage0 core.h $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v4: stage1 core.h $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v4 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v4

clean:
	rm -f whisper-v4 stage0 stage1

.PHONY: all clean
