CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm macro.scm

all: whisper-v5

stage0: $(SRC_FILES)
	./whisper-v4 whisper.scm -o stage0

stage1: stage0 core.h $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v5: stage1 core.h $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v5 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v5

clean:
	rm -f whisper-v5 stage0 stage1

.PHONY: all clean
