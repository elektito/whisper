;; main.scm imports (whisper core) for three primcalls that whisper's
;; (whisper) does not export. this stands in for them.
(define-library (whisper core)
  (import (whisper))
  (export make-empty-environment
          environment-bind!
          environment-lookup)
  (begin
    ;; entries are (kind . value) pairs, as whisper's environment-lookup
    ;; returns them.
    (define (make-empty-environment)
      (make-eq-hash-table))

    (define (environment-bind! env name kind value)
      (unless (memq kind '(value macro special aux primcall alias))
        (error (string-append "environment-bind!: invalid kind: "
                              (symbol->string kind))))
      (hash-table-set! env name (cons kind value)))

    (define (environment-lookup env name)
      (hash-table-ref/default env name #f))))
