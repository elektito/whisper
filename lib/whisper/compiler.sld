(define-library (whisper compiler)
  (import (whisper core) (whisper))
  (include "compiler/whisper.scm")
  (include "repl.scm")
  (begin
    ;; default search paths, from the environment variables only.
    ;; main.scm calls init-compiler again with its -L and -I
    ;; directories.
    (init-compiler '() '()))
  ;; bootstrap-scheme/whisper/compiler.sld has a copy of this list too.
  ;; they need to be updated together.
  (export environment
          eval
          interaction-environment
          load
          null-environment
          run-repl
          scheme-report-environment

          ;; used by main.scm
          init-compiler
          compile-file
          expand-for-display))
