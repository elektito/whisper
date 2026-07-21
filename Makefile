CFLAGS ?=

COMPILER_SRC = whisper.scm utils.scm format.scm qq.scm expand.scm syntax-rules.scm
SRC_FILES = $(COMPILER_SRC) main.scm

LIB_EXPORT_FILES = lib/scheme-base-exports.scm \
                   lib/scheme-cxr-exports.scm \
                   lib/scheme-char-exports.scm \
                   lib/scheme-case-lambda-exports.scm \
                   lib/scheme-file-exports.scm \
                   lib/scheme-process-context-exports.scm \
                   lib/scheme-write-exports.scm

all: whisper-v18

stage0: $(SRC_FILES)
	./whisper-v17 main.scm -o stage0 -C prev

stage1: stage0 core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)"

whisper-v18: stage1 core.h core.c $(SRC_FILES)
	./stage1 main.scm -o whisper-v18 -f "-Wl,-s $(CFLAGS)"
	diff stage1 whisper-v18

test: whisper-v18 lib/whisper.manifest
	./whisper-v18 test.scm -t -r -L lib

matrix: whisper-v18 lib/whisper.manifest
	./whisper-v18 main.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o \
		&& /tmp/b.$$o test.scm -t -r -L lib || exit 1; \
	done

lib/whisper.manifest lib/whisper.so lib/whisper.a &: whisper-v18 lib/whisper.sld utils.scm format.scm $(LIB_EXPORT_FILES)
	./whisper-v18 lib/whisper.sld -l -o lib/whisper

lib/scheme.manifest lib/scheme.so lib/scheme.a &: whisper-v18 lib/scheme.sld lib/whisper.manifest $(LIB_EXPORT_FILES)
	./whisper-v18 lib/scheme.sld -l -o lib/scheme -L lib

lib/eval.manifest lib/eval.so lib/eval.a &: whisper-v18 lib/scheme-eval.sld $(COMPILER_SRC)
	./whisper-v18 lib/scheme-eval.sld -l -o lib/eval -L lib

libs: lib/whisper.manifest lib/scheme.manifest lib/eval.manifest

clean:
	rm -f whisper-v18 stage0 stage1 libwhisper.a
	rm -f lib/whisper.manifest lib/whisper.so lib/whisper.a
	rm -f lib/scheme.manifest lib/scheme.so lib/scheme.a
	rm -f lib/eval.manifest lib/eval.so lib/eval.a

.PHONY: all clean test matrix libs
