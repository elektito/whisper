;; the repl, interaction-environment, load, and the R5RS environment
;; procedures. included by (whisper compiler) and re-exported by
;; (whisper repl), (scheme repl), (scheme load), and (scheme r5rs).

(define *interaction-environment* #f)

;; created on first use, so programs that never ask for it don't pay for
;; loading its libraries.
(define (interaction-environment)
  (unless *interaction-environment*
    (set! *interaction-environment*
          (environment '(whisper) '(scheme eval) '(scheme repl))))
  *interaction-environment*)

(define (check-r5rs-version who version)
  (unless (eqv? version 5)
    (error (string-append who " only supports version 5") version)))

(define (scheme-report-environment version)
  (check-r5rs-version "scheme-report-environment" version)
  (environment '(scheme r5rs)))

;; only the syntactic keywords of r5rs
(define (null-environment version)
  (check-r5rs-version "null-environment" version)
  (environment '(only (scheme r5rs)
                      quote lambda if set! cond case and or let let* letrec
                      begin do delay quasiquote unquote unquote-splicing
                      define define-syntax let-syntax letrec-syntax
                      syntax-rules else => ...)))

;; reads the expressions in filename and evaluates them in order. each
;; one is compiled separately, so this is slow for long files.
(define load
  (case-lambda
   ((filename)
    (load filename (interaction-environment)))
   ((filename env)
    (call-with-input-file filename
      (lambda (port)
        (parameterize ((reader-state (new-reader-state filename)))
          (let loop ()
            (let ((expr (read port)))
              (unless (eof-object? expr)
                (eval expr env)
                (loop))))))))))

(define (report-repl-error e)
  (display "error: " (current-error-port))
  (if (error-object? e)
      (display (error-object-message e) (current-error-port))
      (write e (current-error-port)))
  (newline (current-error-port)))

(define (write-repl-results vals)
  (for-each (lambda (v)
              (unless (void? v)
                (write v)
                (newline)))
            vals))

;; reads expressions from the current input port and evaluates them in
;; the interaction environment until end of file.
(define (run-repl)
  (let ((env (interaction-environment)))
    (let loop ()
      (display "> ")
      (let ((result
             (parameterize ((reader-state (new-reader-state "<repl>")))
               (guard (e (else (report-repl-error e) #f))
                 (let ((expr (read (current-input-port))))
                   (if (eof-object? expr)
                       expr
                       (begin
                         (call-with-values (lambda () (eval expr env))
                           (lambda vals (write-repl-results vals)))
                         #t)))))))
        (if (eof-object? result)
            ;; print a newline before returning, so the shell prompt
            ;; doesn't appear on the same line
            (newline)
            (loop))))))
