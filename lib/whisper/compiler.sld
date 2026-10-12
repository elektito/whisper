(define-library (whisper compiler)
  (import (whisper core) (whisper))
  (include "compiler/whisper.scm")
  (include "repl.scm")
  (begin
    ;; default search paths, from the environment variables only. main.scm
    ;; calls init-compiler again with its -L and -I directories.
    (init-compiler '() '()))
  ;; bootstrap-scheme/whisper/compiler.sld has a copy of this list
  (export environment
          eval
          interaction-environment
          run-repl

          ;; used by main.scm
          init-compiler
          create-program
          compile-program
          output-program-code
          program-debug
          program-debug-set!
          program-is-test-suite-set!
          program-library-mode-set!
          build-compile-cmd
          write-manifest
          temp-filename
          new-expand-root-env
          expand-top-level-form
          de-identifier))
