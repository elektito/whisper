;; main.scm imports the compiler as (whisper compiler). this stands in
;; for lib/whisper/compiler.sld.
;;
;; guile resolves include and include-library-declarations against this
;; file's path relative to the load path (whisper/compiler.sld), opened
;; from the current directory, so neither can reach the repo files from
;; here. include-from-path searches the load path instead, which has
;; bootstrap-scheme on it.
(define-library (whisper compiler)
  (import (whisper) (whisper core))
  (cond-expand
   (guile
    (import (only (guile) include-from-path))
    (begin
      (include-from-path "../lib/whisper/compiler/whisper.scm")
      (include-from-path "../lib/whisper/repl.scm"))))
  ;; a copy of the list in lib/whisper/compiler.sld
  (export environment
          eval
          interaction-environment
          run-repl
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
