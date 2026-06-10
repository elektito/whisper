CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm macro.scm preprocess.scm

all: whisper-v8

stage0: $(SRC_FILES)
	./whisper-v7 whisper.scm -f "core.c" -o stage0

stage1: stage0 core.h $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v8: stage1 core.h $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v8 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v8

test: whisper-v8
	./whisper-v8 test.scm -t -r

matrix: whisper-v8
	./whisper-v8 whisper.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c \
		&& /tmp/b.$$o whisper.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r || exit 1; \
	done

clean:
	rm -f whisper-v8 stage0 stage1

.PHONY: all clean test matrix
