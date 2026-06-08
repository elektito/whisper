CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm macro.scm preprocess.scm

all: whisper-v6

stage0: $(SRC_FILES)
	./whisper-v5 whisper.scm -o stage0

stage1: stage0 core.h $(SRC_FILES)
	./stage0 whisper.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v6: stage1 core.h $(SRC_FILES)
	./stage1 whisper.scm -o whisper-v6 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v6

test: whisper-v6
	./whisper-v6 test.scm -t -r

matrix: whisper-v6
	./whisper-v6 whisper.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c \
		&& /tmp/b.$$o whisper.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r || exit 1; \
	done

clean:
	rm -f whisper-v6 stage0 stage1

.PHONY: all clean test matrix
