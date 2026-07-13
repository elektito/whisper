#;(import (whisper core))

(include "whisper.scm")

(define (build-compile-cmd-from-args cc own-cflags args)
  (build-compile-cmd cc
                     (cmdline-library-mode args)
                     (format "~a ~a" own-cflags (cmdline-cflags args))
                     (cmdline-c-file args)
                     (cmdline-executable-file args)
                     (cmdline-core-path args)
                     (cmdline-archives args)))

;;;;;; command-line parsing ;;;;;;

(define-record-type <cmdline>
  (make-cmdline just-compile run output-file input-file test c-file executable-file delete-executable debug cflags library-mode core-path archives)
  cmdline?
  (just-compile cmdline-just-compile cmdline-just-compile-set!)
  (run cmdline-run cmdline-run-set!)
  (output-file cmdline-output-file cmdline-output-file-set!)
  (input-file cmdline-input-file cmdline-input-file-set!)
  (test cmdline-test cmdline-test-set!)
  (c-file cmdline-c-file cmdline-c-file-set!)
  (executable-file cmdline-executable-file cmdline-executable-file-set!)
  (delete-executable cmdline-delete-executable cmdline-delete-executable-set!)
  (debug cmdline-debug cmdline-debug-set!)
  (cflags cmdline-cflags cmdline-cflags-set!)
  (library-mode cmdline-library-mode cmdline-library-mode-set!)
  (core-path cmdline-core-path cmdline-core-path-set!)
  (archives cmdline-archives cmdline-archives-set!))

(define (create-cmdline-args)
  (make-cmdline #f  ; just compile
                #f  ; run
                #f  ; output file
                #f  ; input file
                #f  ; test
                #f  ; c output file
                #f  ; executable output file
                #f  ; delete executable
                #f  ; debug
                ""  ; cflags
                #f  ; library mode
                "." ; core path
                '() ; archives
                ))

(define (command-line-error fmt . args)
  (apply format (current-error-port) fmt args)
  (newline (current-error-port))
  (exit 1))

(define (parse-command-line-args)
  (let ((args (create-cmdline-args)))
    (let loop ((cl (cdr (command-line))))
      (cond ((null? cl) args)
            ((string=? (car cl) "-h")
             (print-usage))
            ((string=? (car cl) "-?")
             (print-usage))
            ((string=? (car cl) "-c")
             (cmdline-just-compile-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-r")
             (cmdline-run-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-t")
             (cmdline-test-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-g")
             (cmdline-debug-set! args #t)
             (loop (cdr cl)))
            ((string=? (car cl) "-l")
             (cmdline-library-mode-set! args 'so)
             (loop (cdr cl)))
            ((string=? (car cl) "-L")
             (cmdline-library-mode-set! args 'static)
             (loop (cdr cl)))
            ((string=? (car cl) "-C")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -C")
                 (begin
                   (cmdline-core-path-set! args (cadr cl))
                   (loop (cddr cl)))))
            ((string=? (car cl) "-a")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -a")
                 (begin
                   (cmdline-archives-set! args (cons (cadr cl) (cmdline-archives args)))
                   (loop (cddr cl)))))
            ((string=? (car cl) "-f")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -f")
                 (begin
                   (cmdline-cflags-set! args (cadr cl))
                   (loop (cddr cl)))))
            ((string=? (car cl) "-o")
             (if (null? (cdr cl))
                 (command-line-error "missing argument to -o")
                 (begin
                   (cmdline-output-file-set! args (cadr cl))
                   (loop (cddr cl)))))
            (else (if (cmdline-input-file args)
                      (command-line-error "unexpected argument: ~a" (car cl))
                      (begin
                        (cmdline-input-file-set! args (car cl))
                        (loop (cdr cl)))))))))

(define (postprocess-cmdline args)
  (if (and (cmdline-just-compile args) (cmdline-run args))
      (command-line-error "-r and -c are mutually exclusive"))
  (if (and (cmdline-library-mode args) (cmdline-run args))
      (command-line-error "-l/-L and -r are mutually exclusive"))
  (if (cmdline-output-file args)
      (if (cmdline-just-compile args)
          (cmdline-c-file-set! args (cmdline-output-file args))
          (begin
            (cmdline-c-file-set! args (string-append (temp-filename) ".c"))
            (cmdline-executable-file-set! args (cmdline-output-file args))))
      (begin
        (cmdline-delete-executable-set! args #t)
        (if (cmdline-just-compile args)
            (cmdline-output-file-set! args "b.c")
            (if (cmdline-run args)
                (begin
                  (cmdline-output-file-set! args (temp-filename))
                  (cmdline-delete-executable-set! args #t))
                (case (cmdline-library-mode args)
                  ((so)     (cmdline-output-file-set! args "b.so"))
                  ((static) (cmdline-output-file-set! args "b.a"))
                  (else     (cmdline-output-file-set! args "b.out")))))))
  (if (cmdline-just-compile args)
      (cmdline-c-file-set! args (cmdline-output-file args))
      (begin
        (cmdline-c-file-set! args (string-append (temp-filename) ".c"))
        (cmdline-executable-file-set! args (cmdline-output-file args)))))

(define (print-usage)
  (format (current-error-port) "usage: ~a [input-file] [-r] [-c] [-l] [-L] [-C core-path] [-o output-file] [-f cflags] [-a archive] [-g]

 -r\tcompile and run the program
 -c\tonly compile a c file
 -l\tcompile as a shared library (.so)
 -L\tcompile as a static library (.a)
 -C\tpath to core files (core.h and core.c).
 -o\tthe name of the output file. defaults to b.c, b.out, b.so, or b.a
\tdepending on -c, -l, -L, or neither.
 -f\tuse the given options when invoking the C compiler
 -a\tlink a static archive (may be repeated)
 -t\tcompile the program as a test suite
 -g\tadd debug instrumentation
 if no input file is given, a repl is started.
" (car (command-line)))
  (exit 0)
  )

;;;;;; repl ;;;;;;

(define (repl env)
  (display "> ")
  (let ((expr (read (current-input-port))))
    (unless (eof-object? expr)
      (let ((result (eval expr env)))
        (unless (void? result)
          (write result)
          (newline)))
      (repl env))))

;;;;;; main ;;;;;;

(let ((args (parse-command-line-args)))
  (init-find-library)
  (when (not (cmdline-input-file args))
    (repl (make-environment))
    (exit 0))
  (postprocess-cmdline args)
  (let ((port (open-input-file (cmdline-input-file args))))
    (let ((program (create-program port (make-empty-environment) #t)))
      (if (cmdline-test args)
          (program-is-test-suite-set! program #t))
      (if (cmdline-debug args)
          (program-debug-set! program #t))
      (when (cmdline-library-mode args)
        (program-library-mode-set! program (cmdline-library-mode args)))
      (compile-program program)
      (output-program-code program (cmdline-c-file args))
      (if (not (cmdline-just-compile args))
          (let ((cc (get-environment-variable "CC")))
            (let ((cc (if cc cc "gcc"))
                  (own-cflags (if (program-debug program) " -DDEBUG" "")))
              (let ((cmd (build-compile-cmd-from-args cc own-cflags args)))
                (let ((ret (system cmd)))
                  (delete-file (cmdline-c-file args))
                  (if (not (zero? ret))
                      (begin
                        (format (current-error-port) "program exited with error code: ~a\n" ret)
                        (exit ret))))))))
      (if (cmdline-run args)
          (let ((cmd (format "$(realpath ~a)" (cmdline-executable-file args))))
            (let ((ret (system cmd)))
              (if (cmdline-delete-executable args)
                  (delete-file (cmdline-executable-file args)))
              (exit ret)))))))
