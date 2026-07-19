CFLAGS ?=

SRC_FILES = whisper.scm utils.scm format.scm qq.scm expand.scm syntax-rules.scm main.scm

LIB_EXPORT_FILES = lib/scheme-base-exports.scm \
                   lib/scheme-cxr-exports.scm \
                   lib/scheme-char-exports.scm \
                   lib/scheme-case-lambda-exports.scm \
                   lib/scheme-file-exports.scm \
                   lib/scheme-process-context-exports.scm \
                   lib/scheme-write-exports.scm

all: whisper-v16

stage0: $(SRC_FILES)
	./whisper-v15 main.scm -o stage0 -C prev

stage1: stage0 core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v16: stage1 core.h core.c $(SRC_FILES)
	./stage1 main.scm -o whisper-v16 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v16

test: whisper-v16
	./whisper-v16 test.scm -t -r

matrix: whisper-v16
	./whisper-v16 main.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r || exit 1; \
	done

libwhisper.a: whisper-v16 $(SRC_FILES) core.h core.c
	./whisper-v16 whisper.scm -L -o libwhisper.a

lib/whisper.manifest lib/whisper.so lib/whisper.a &: whisper-v16 lib/whisper.sld utils.scm format.scm $(LIB_EXPORT_FILES)
	./whisper-v16 lib/whisper.sld -l -o lib/whisper

lib/scheme.manifest lib/scheme.so lib/scheme.a &: whisper-v16 lib/scheme.sld lib/whisper.manifest $(LIB_EXPORT_FILES)
	./whisper-v16 lib/scheme.sld -l -o lib/scheme -L lib

libs: lib/whisper.manifest lib/scheme.manifest

clean:
	rm -f whisper-v16 stage0 stage1 libwhisper.a
	rm -f lib/whisper.manifest lib/whisper.so lib/whisper.a
	rm -f lib/scheme.manifest lib/scheme.so lib/scheme.a

.PHONY: all clean test matrix libs
