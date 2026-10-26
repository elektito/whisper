;; main.scm and the compiler import (whisper core) for primcalls that
;; whisper's (whisper) does not export. this stands in for them.
(define-library (whisper core)
  (import (whisper))
  (export make-empty-environment
          environment-bind!
          environment-lookup
          port-fold-case-set!)
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
      (hash-table-ref/default env name #f))

    ;; guile has no way to set this on a port. the compiler only sets it
    ;; for include-ci, which its own sources never use, so fail loudly
    ;; instead of reading a file unfolded.
    (define (port-fold-case-set! port fold-case?)
      (when fold-case?
        (error "port-fold-case-set!: not supported when bootstrapping from another scheme")))))
