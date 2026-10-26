CURRENT_V = 37
PREV_V = $(shell echo $$(($(CURRENT_V) - 1)))

CURRENT = whisper-v$(CURRENT_V)
PREV = whisper-v$(PREV_V)

CFLAGS ?=

COMPILER_SRC = lib/whisper/compiler/whisper.scm lib/whisper/compiler/qq.scm lib/whisper/compiler/expand.scm lib/whisper/compiler/syntax-rules.scm lib/whisper/shared.scm
COMPILER_LIB_SRC = $(COMPILER_SRC) lib/whisper/compiler.sld lib/whisper/repl.scm

LIB_EXPORT_FILES = lib/whisper/scheme-base-exports.scm \
                   lib/whisper/scheme-cxr-exports.scm \
                   lib/whisper/scheme-char-exports.scm \
                   lib/whisper/scheme-case-lambda-exports.scm \
                   lib/whisper/scheme-file-exports.scm \
                   lib/whisper/scheme-lazy-exports.scm \
                   lib/whisper/scheme-process-context-exports.scm \
                   lib/whisper/scheme-read-exports.scm \
                   lib/whisper/scheme-time-exports.scm \
                   lib/whisper/scheme-write-exports.scm

WHISPER_LIB_SRC = lib/whisper/stdlib.scm lib/whisper/format.scm lib/whisper/read.scm lib/whisper/write.scm lib/whisper/shared.scm lib/whisper/whisper.sld $(LIB_EXPORT_FILES)

RAYLIB_SRC = vendor/raylib/src

all: $(CURRENT)

# each bootstrap stage needs its own (whisper) and (whisper compiler)
# libraries built by the previous stage before it can compile main.scm.
# stageN-lib holds the libraries used to build stageN's compiler.
stage0-lib/whisper.manifest stage0-lib/whisper.so stage0-lib/whisper.a &: prev/core.h prev/core.c $(WHISPER_LIB_SRC)
	mkdir -p stage0-lib
	./$(PREV) lib/whisper/whisper.sld -l -o stage0-lib/whisper -C prev

stage0-lib/compiler.manifest stage0-lib/compiler.so stage0-lib/compiler.a &: stage0-lib/whisper.manifest prev/core.h prev/core.c $(COMPILER_LIB_SRC)
	./$(PREV) lib/whisper/compiler.sld -l -o stage0-lib/compiler -C prev -L stage0-lib

stage0: stage0-lib/compiler.manifest prev/core.h prev/core.c main.scm
	./$(PREV) main.scm -o stage0 -C prev -L stage0-lib

stage1-lib/whisper.manifest stage1-lib/whisper.so stage1-lib/whisper.a &: stage0 core.h $(WHISPER_LIB_SRC)
	mkdir -p stage1-lib
	./stage0 lib/whisper/whisper.sld -l -o stage1-lib/whisper -f "$(CFLAGS)"

stage1-lib/compiler.manifest stage1-lib/compiler.so stage1-lib/compiler.a &: stage0 stage1-lib/whisper.manifest core.h $(COMPILER_LIB_SRC)
	./stage0 lib/whisper/compiler.sld -l -o stage1-lib/compiler -L stage1-lib -f "$(CFLAGS)"

stage1: stage0 stage1-lib/compiler.manifest core.h core.c main.scm
	./stage0 main.scm -o stage1 -f "-Wl,-s $(CFLAGS)" -L stage1-lib

stage2-lib/whisper.manifest stage2-lib/whisper.so stage2-lib/whisper.a &: stage1 core.h $(WHISPER_LIB_SRC)
	mkdir -p stage2-lib
	./stage1 lib/whisper/whisper.sld -l -o stage2-lib/whisper -f "$(CFLAGS)"

stage2-lib/compiler.manifest stage2-lib/compiler.so stage2-lib/compiler.a &: stage1 stage2-lib/whisper.manifest core.h $(COMPILER_LIB_SRC)
	./stage1 lib/whisper/compiler.sld -l -o stage2-lib/compiler -L stage2-lib -f "$(CFLAGS)"

$(CURRENT): stage1 stage2-lib/compiler.manifest core.h core.c main.scm
	./stage1 main.scm -o $(CURRENT) -f "-Wl,-s $(CFLAGS)" -L stage2-lib
	diff stage1 $(CURRENT)

# extra arguments for the test suite, e.g. make test ARGS=-v
ARGS ?=

test: $(CURRENT) libs
	WHISPER_LIBRARY_PATH=lib ./$(CURRENT) test.scm -t -r -L lib -- $(ARGS)

time: $(CURRENT) lib/whisper.manifest
	bash -c 'time (./$(CURRENT) lib/whisper/whisper.sld -l -c -o /tmp/time-whisper.c \
		&& ./$(CURRENT) lib/whisper/compiler.sld -l -c -o /tmp/time-compiler.c -L lib)'

matrix: $(CURRENT) libs
	./$(CURRENT) main.scm -c -o /tmp/b.c -L lib
	@for o in 0 1 2 3; do \
		echo "--- O$$o ---"; \
		mkdir -p /tmp/matrix-lib.$$o \
		&& ./$(CURRENT) lib/whisper/whisper.sld -l -o /tmp/matrix-lib.$$o/whisper -f -O$$o \
		&& ./$(CURRENT) lib/whisper/compiler.sld -l -o /tmp/matrix-lib.$$o/compiler -L /tmp/matrix-lib.$$o -f -O$$o \
		&& gcc -O$$o -Wl,-s -I. -ldl -Wl,--export-dynamic -o /tmp/b.$$o /tmp/b.c -Wl,--whole-archive /tmp/matrix-lib.$$o/whisper.a /tmp/matrix-lib.$$o/compiler.a -Wl,--no-whole-archive core.c \
		&& /tmp/b.$$o main.scm -o /tmp/out.$$o -L lib \
		&& WHISPER_LIBRARY_PATH=lib /tmp/b.$$o test.scm -t -r -L lib || exit 1; \
	done

lib/whisper.manifest lib/whisper.so lib/whisper.a &: $(CURRENT) lib/whisper/whisper.sld $(WHISPER_LIB_SRC)
	./$(CURRENT) lib/whisper/whisper.sld -l -o lib/whisper -f "$(CFLAGS)"

lib/scheme.manifest lib/scheme.so lib/scheme.a &: $(CURRENT) lib/whisper/scheme.sld lib/whisper.manifest $(LIB_EXPORT_FILES)
	./$(CURRENT) lib/whisper/scheme.sld -l -o lib/scheme -L lib -f "$(CFLAGS)"

lib/compiler.manifest lib/compiler.so lib/compiler.a &: $(CURRENT) lib/whisper.manifest $(COMPILER_LIB_SRC)
	./$(CURRENT) lib/whisper/compiler.sld -l -o lib/compiler -L lib -f "$(CFLAGS)"

lib/eval.manifest lib/eval.so lib/eval.a &: $(CURRENT) lib/whisper/scheme-eval.sld lib/compiler.manifest
	./$(CURRENT) lib/whisper/scheme-eval.sld -l -o lib/eval -L lib -f "$(CFLAGS)"

lib/scheme-repl.manifest lib/scheme-repl.so lib/scheme-repl.a &: $(CURRENT) lib/whisper/scheme-repl.sld lib/compiler.manifest
	./$(CURRENT) lib/whisper/scheme-repl.sld -l -o lib/scheme-repl -L lib -f "$(CFLAGS)"

lib/whisper-repl.manifest lib/whisper-repl.so lib/whisper-repl.a &: $(CURRENT) lib/whisper/whisper-repl.sld lib/compiler.manifest
	./$(CURRENT) lib/whisper/whisper-repl.sld -l -o lib/whisper-repl -L lib -f "$(CFLAGS)"

lib/libraylib.a lib/libraylib.so lib/libraylib.so.600 &:
	$(MAKE) -C $(RAYLIB_SRC) RAYLIB_LIBTYPE=STATIC
	$(MAKE) -C $(RAYLIB_SRC) RAYLIB_LIBTYPE=SHARED

	cp $(RAYLIB_SRC)/libraylib.a lib/
	cp $(RAYLIB_SRC)/libraylib.so lib/
	cp $(RAYLIB_SRC)/libraylib.so.600 lib/

lib/raylib.manifest lib/raylib.so lib/raylib.a &: $(CURRENT) lib/raylib/raylib.sld lib/raylib/raylib.c lib/libraylib.a lib/libraylib.so
	./$(CURRENT) lib/raylib/raylib.sld -l -o lib/raylib -f '-I lib -I $(RAYLIB_SRC) $(CFLAGS)'

lib/srfi-1.manifest lib/srfi-1.so lib/srfi-1.a &: $(CURRENT) lib/srfi-8.manifest lib/srfi-1-list/srfi-1.sld lib/srfi-1-list/srfi-1.scm
	./$(CURRENT) lib/srfi-1-list/srfi-1.sld -l -o lib/srfi-1 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-8.manifest lib/srfi-8.so lib/srfi-8.a &: $(CURRENT) lib/srfi-8-receive/srfi-8.sld lib/srfi-8-receive/srfi-8.scm
	./$(CURRENT) lib/srfi-8-receive/srfi-8.sld -l -o lib/srfi-8 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-48.manifest lib/srfi-48.so lib/srfi-48.a &: $(CURRENT) lib/whisper.manifest lib/srfi-48-format/srfi-48.sld
	./$(CURRENT) lib/srfi-48-format/srfi-48.sld -l -o lib/srfi-48 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-69.manifest lib/srfi-69.so lib/srfi-69.a &: $(CURRENT) lib/whisper.manifest lib/srfi-69-hash-tables/srfi-69.sld
	./$(CURRENT) lib/srfi-69-hash-tables/srfi-69.sld -l -o lib/srfi-69 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-111.manifest lib/srfi-111.so lib/srfi-111.a &: $(CURRENT) lib/whisper.manifest lib/srfi-111-boxes/srfi-111.sld
	./$(CURRENT) lib/srfi-111-boxes/srfi-111.sld -l -o lib/srfi-111 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-133.manifest lib/srfi-133.so lib/srfi-133.a &: $(CURRENT) lib/whisper.manifest lib/srfi-133-vectors/srfi-133.sld lib/srfi-133-vectors/srfi-133.scm  
	./$(CURRENT) lib/srfi-133-vectors/srfi-133.sld -l -o lib/srfi-133 -L lib -f '-I lib $(CFLAGS)'

lib/srfi-151.manifest lib/srfi-151.so lib/srfi-151.a &: $(CURRENT) lib/srfi-151-bitwise/srfi-151.sld lib/srfi-151-bitwise/bitwise.c lib/srfi-151-bitwise/bitwise-33.scm lib/srfi-151-bitwise/bitwise-60.scm lib/srfi-151-bitwise/bitwise-other.scm
	./$(CURRENT) lib/srfi-151-bitwise/srfi-151.sld -l -o lib/srfi-151 -L lib -f '-I lib $(CFLAGS)'

libs: lib/whisper.manifest lib/scheme.manifest lib/compiler.manifest lib/eval.manifest lib/scheme-repl.manifest lib/whisper-repl.manifest lib/raylib.manifest lib/srfi-1.manifest lib/srfi-8.manifest lib/srfi-48.manifest lib/srfi-69.manifest lib/srfi-111.manifest lib/srfi-133.manifest lib/srfi-151.manifest

clean:
	rm -rf $(CURRENT) stage0 stage1 stage0-lib stage1-lib stage2-lib
	rm -f lib/whisper.manifest lib/whisper.so lib/whisper.a
	rm -f lib/scheme.manifest lib/scheme.so lib/scheme.a
	rm -f lib/compiler.manifest lib/compiler.so lib/compiler.a
	rm -f lib/eval.manifest lib/eval.so lib/eval.a
	rm -f lib/scheme-repl.manifest lib/scheme-repl.so lib/scheme-repl.a
	rm -f lib/whisper-repl.manifest lib/whisper-repl.so lib/whisper-repl.a
	rm -f lib/raylib.manifest lib/raylib.so lib/raylib.a
	rm -f lib/srfi-48.manifest lib/srfi-48.so lib/srfi-48.a
	rm -f lib/srfi-69.manifest lib/srfi-69.so lib/srfi-69.a
	rm -f lib/srfi-111.manifest lib/srfi-111.so lib/srfi-111.a
	rm -f lib/srfi-133.manifest lib/srfi-133.so lib/srfi-133.a
	rm -f lib/srfi-151.manifest lib/srfi-151.so lib/srfi-151.a
	$(MAKE) -C $(RAYLIB_SRC) clean

.PHONY: all clean test time matrix libs
