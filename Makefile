CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm expand.scm syntax-rules.scm main.scm

all: whisper-v15

stage0: $(SRC_FILES)
	./whisper-v14 main.scm -o stage0 -C prev

stage1: stage0 core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v15: stage1 core.h core.c $(SRC_FILES)
	./stage1 main.scm -o whisper-v15 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v15

test: whisper-v15
	./whisper-v15 test.scm -t -r

matrix: whisper-v15
	./whisper-v15 main.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r || exit 1; \
	done

libwhisper.a: whisper-v15 $(SRC_FILES) core.h core.c
	./whisper-v15 whisper.scm -L -o libwhisper.a

clean:
	rm -f whisper-v15 stage0 stage1 libwhisper.a

.PHONY: all clean test matrix
