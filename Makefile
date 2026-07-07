CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm expand.scm syntax-rules.scm main.scm

all: whisper-v14

stage0: $(SRC_FILES)
	./whisper-v13 main.scm -o stage0 -C prev

stage1: stage0 core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v14: stage1 core.h core.c $(SRC_FILES)
	./stage1 main.scm -o whisper-v14 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v14

test: whisper-v14
	./whisper-v14 test.scm -t -r

matrix: whisper-v14
	./whisper-v14 main.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r || exit 1; \
	done

libwhisper.a: whisper-v14 $(SRC_FILES) core.h core.c
	./whisper-v14 whisper.scm -L -o libwhisper.a

clean:
	rm -f whisper-v14 stage0 stage1 libwhisper.a

.PHONY: all clean test matrix
