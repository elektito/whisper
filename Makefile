CURRENT_V = 31
PREV_V = $(shell echo $$(($(CURRENT_V) - 1)))

CURRENT = whisper-v$(CURRENT_V)
PREV = whisper-v$(PREV_V)

CFLAGS ?=

COMPILER_SRC = whisper.scm qq.scm expand.scm syntax-rules.scm
SRC_FILES = $(COMPILER_SRC) main.scm

LIB_EXPORT_FILES = lib/whisper/scheme-base-exports.scm \
                   lib/whisper/scheme-cxr-exports.scm \
                   lib/whisper/scheme-char-exports.scm \
                   lib/whisper/scheme-case-lambda-exports.scm \
                   lib/whisper/scheme-file-exports.scm \
                   lib/whisper/scheme-process-context-exports.scm \
                   lib/whisper/scheme-write-exports.scm \
				   lib/whisper/scheme-read-exports.scm

WHISPER_LIB_SRC = lib/whisper/stdlib.scm lib/whisper/format.scm lib/whisper/read.scm lib/whisper/whisper.sld $(LIB_EXPORT_FILES)

RAYLIB_SRC = vendor/raylib/src

all: $(CURRENT)

# each bootstrap stage needs its own (whisper) library built by the
# previous stage before it can compile main.scm. stageN-lib is the
# library used to build stageN's compiler.
stage0-lib/whisper.manifest stage0-lib/whisper.so stage0-lib/whisper.a &: $(WHISPER_LIB_SRC)
	mkdir -p stage0-lib
	./$(PREV) lib/whisper/whisper.sld -l -o stage0-lib/whisper -C prev

stage0: stage0-lib/whisper.manifest $(SRC_FILES)
	./$(PREV) main.scm -o stage0 -C prev -L stage0-lib

stage1-lib/whisper.manifest stage1-lib/whisper.so stage1-lib/whisper.a &: stage0 $(WHISPER_LIB_SRC)
	mkdir -p stage1-lib
	./stage0 lib/whisper/whisper.sld -l -o stage1-lib/whisper

stage1: stage0 stage1-lib/whisper.manifest core.h core.c $(SRC_FILES)
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)" -L stage1-lib

stage2-lib/whisper.manifest stage2-lib/whisper.so stage2-lib/whisper.a &: stage1 $(WHISPER_LIB_SRC)
	mkdir -p stage2-lib
	./stage1 lib/whisper/whisper.sld -l -o stage2-lib/whisper

$(CURRENT): stage1 stage2-lib/whisper.manifest core.h core.c $(SRC_FILES)
	./stage1 main.scm -o $(CURRENT) -f "-Wl,-s $(CFLAGS)" -L stage2-lib
	diff stage1 $(CURRENT)

test: $(CURRENT) libs
	WHISPER_LIBRARY_PATH=lib ./$(CURRENT) test.scm -t -r -L lib

matrix: $(CURRENT) libs
	./$(CURRENT) main.scm -c -o /tmp/b.c -L lib
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		gcc -O$$o -Wl,-s -I. -ldl -Wl,--export-dynamic -o /tmp/b.$$o /tmp/b.c -Wl,--whole-archive lib/whisper.a -Wl,--no-whole-archive core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o -L lib \
		&& WHISPER_LIBRARY_PATH=lib /tmp/b.$$o test.scm -t -r -L lib || exit 1; \
	done

lib/whisper.manifest lib/whisper.so lib/whisper.a &: $(CURRENT) lib/whisper/whisper.sld $(WHISPER_LIB_SRC)
	./$(CURRENT) lib/whisper/whisper.sld -l -o lib/whisper

lib/scheme.manifest lib/scheme.so lib/scheme.a &: $(CURRENT) lib/whisper/scheme.sld lib/whisper.manifest $(LIB_EXPORT_FILES)
	./$(CURRENT) lib/whisper/scheme.sld -l -o lib/scheme -L lib

lib/eval.manifest lib/eval.so lib/eval.a &: $(CURRENT) lib/whisper/scheme-eval.sld $(COMPILER_SRC)
	./$(CURRENT) lib/whisper/scheme-eval.sld -l -o lib/eval -L lib

lib/libraylib.a lib/libraylib.so lib/libraylib.so.600 &:
	$(MAKE) -C $(RAYLIB_SRC) RAYLIB_LIBTYPE=STATIC
	$(MAKE) -C $(RAYLIB_SRC) RAYLIB_LIBTYPE=SHARED

	cp $(RAYLIB_SRC)/libraylib.a lib/
	cp $(RAYLIB_SRC)/libraylib.so lib/
	cp $(RAYLIB_SRC)/libraylib.so.600 lib/

lib/raylib.manifest lib/raylib.so lib/raylib.a &: $(CURRENT) lib/raylib/raylib.sld lib/raylib/raylib.c lib/libraylib.a lib/libraylib.so
	./$(CURRENT) lib/raylib/raylib.sld -l -o lib/raylib -f '-I lib -I $(RAYLIB_SRC)'

lib/srfi-1.manifest lib/srfi-1.so lib/srfi-1.a &: $(CURRENT) lib/srfi-8.manifest lib/srfi-1-list/srfi-1.sld lib/srfi-1-list/srfi-1.scm
	./$(CURRENT) lib/srfi-1-list/srfi-1.sld -l -o lib/srfi-1 -f '-I lib'

lib/srfi-8.manifest lib/srfi-8.so lib/srfi-8.a &: $(CURRENT) lib/srfi-8-receive/srfi-8.sld lib/srfi-8-receive/srfi-8.scm
	./$(CURRENT) lib/srfi-8-receive/srfi-8.sld -l -o lib/srfi-8 -f '-I lib'

lib/srfi-151.manifest lib/srfi-151.so lib/srfi-151.a &: $(CURRENT) lib/srfi-151-bitwise/srfi-151.sld lib/srfi-151-bitwise/bitwise.c lib/srfi-151-bitwise/bitwise-33.scm lib/srfi-151-bitwise/bitwise-60.scm lib/srfi-151-bitwise/bitwise-other.scm
	./$(CURRENT) lib/srfi-151-bitwise/srfi-151.sld -l -o lib/srfi-151 -f '-I lib'

libs: lib/whisper.manifest lib/scheme.manifest lib/eval.manifest lib/raylib.manifest lib/srfi-1.manifest lib/srfi-8.manifest lib/srfi-151.manifest

clean:
	rm -rf $(CURRENT) stage0 stage1 stage0-lib stage1-lib stage2-lib
	rm -f lib/whisper.manifest lib/whisper.so lib/whisper.a
	rm -f lib/scheme.manifest lib/scheme.so lib/scheme.a
	rm -f lib/eval.manifest lib/eval.so lib/eval.a
	rm -f lib/raylib.manifest lib/raylib.so lib/raylib.a
	rm -f lib/srfi-151.manifest lib/srfi-151.so lib/srfi-151.a
	$(MAKE) -C $(RAYLIB_SRC) clean

.PHONY: all clean test matrix libs
