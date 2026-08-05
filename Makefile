CURRENT_V = 24
PREV_V = $(shell echo $$(($(CURRENT_V) - 1)))

CURRENT = whisper-v$(CURRENT_V)
PREV = whisper-v$(PREV_V)

CFLAGS ?=

COMPILER_SRC = whisper.scm qq.scm expand.scm syntax-rules.scm
SRC_FILES = $(COMPILER_SRC) main.scm

LIB_EXPORT_FILES = lib/scheme-base-exports.scm \
                   lib/scheme-cxr-exports.scm \
                   lib/scheme-char-exports.scm \
                   lib/scheme-case-lambda-exports.scm \
                   lib/scheme-file-exports.scm \
                   lib/scheme-process-context-exports.scm \
                   lib/scheme-write-exports.scm

WHISPER_LIB_SRC = utils.scm format.scm lib/whisper.sld $(LIB_EXPORT_FILES)

all: $(CURRENT)

# each bootstrap stage needs its own (whisper) library built by the
# previous stage before it can compile main.scm. stageN-lib is the
# library used to build stageN's compiler.
stage0-lib/whisper.manifest stage0-lib/whisper.so stage0-lib/whisper.a &: $(WHISPER_LIB_SRC)
	mkdir -p stage0-lib
	./$(PREV) lib/whisper.sld -l -o stage0-lib/whisper -C prev

stage0: stage0-lib/whisper.manifest $(SRC_FILES)
	./$(PREV) main.scm -o stage0 -C prev -L stage0-lib

stage1-lib/whisper.manifest stage1-lib/whisper.so stage1-lib/whisper.a &: stage0 $(WHISPER_LIB_SRC)
	mkdir -p stage1-lib
	./stage0 lib/whisper.sld -l -o stage1-lib/whisper

stage1: stage0 stage1-lib/whisper.manifest core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)" -L stage1-lib

stage2-lib/whisper.manifest stage2-lib/whisper.so stage2-lib/whisper.a &: stage1 $(WHISPER_LIB_SRC)
	mkdir -p stage2-lib
	./stage1 lib/whisper.sld -l -o stage2-lib/whisper

$(CURRENT): stage1 stage2-lib/whisper.manifest core.h core.c $(SRC_FILES)
	./stage1 main.scm -o $(CURRENT) -f "-Wl,-s $(CFLAGS)" -L stage2-lib
	diff stage1 $(CURRENT)

test: $(CURRENT) libs
	WHISPER_LIBRARY_PATH=lib ./$(CURRENT) test.scm -t -r -L lib

matrix: $(CURRENT) libs
	./$(CURRENT) main.scm -c -o /tmp/b.c
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -o /tmp/b.$$o /tmp/b.c core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o \
		&& WHISPER_LIBRARY_PATH=lib /tmp/b.$$o test.scm -t -r -L lib || exit 1; \
	done

lib/whisper.manifest lib/whisper.so lib/whisper.a &: $(CURRENT) lib/whisper.sld utils.scm format.scm $(LIB_EXPORT_FILES)
	./$(CURRENT) lib/whisper.sld -l -o lib/whisper

lib/scheme.manifest lib/scheme.so lib/scheme.a &: $(CURRENT) lib/scheme.sld lib/whisper.manifest $(LIB_EXPORT_FILES)
	./$(CURRENT) lib/scheme.sld -l -o lib/scheme -L lib

lib/eval.manifest lib/eval.so lib/eval.a &: $(CURRENT) lib/scheme-eval.sld $(COMPILER_SRC)
	./$(CURRENT) lib/scheme-eval.sld -l -o lib/eval -L lib

libs: lib/whisper.manifest lib/scheme.manifest lib/eval.manifest

clean:
	rm -f $(CURRENT) stage0 stage1 libwhisper.a
	rm -f lib/whisper.manifest lib/whisper.so lib/whisper.a
	rm -f lib/scheme.manifest lib/scheme.so lib/scheme.a
	rm -f lib/eval.manifest lib/eval.so lib/eval.a

.PHONY: all clean test matrix libs
