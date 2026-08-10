(define-library (whisper)
  (import (whisper core))
  (include "stdlib.scm")
  (include "format.scm")

  (include-library-declarations "scheme-base-exports.scm")
  (include-library-declarations "scheme-cxr-exports.scm")
  (include-library-declarations "scheme-char-exports.scm")
  (include-library-declarations "scheme-case-lambda-exports.scm")
  (include-library-declarations "scheme-file-exports.scm")
  (include-library-declarations "scheme-process-context-exports.scm")
  (include-library-declarations "scheme-write-exports.scm")

  (export atom?
          list*
          last
          any?
          all?
          filter
          iota
          pairwise
          !=
          string-join
          string-split
          string-suffix?
          string-append-char
          string<?
          string<=?
          string>?
          string>=?
          sort
          print
          format
          pretty-print

          gensym
          box
          box?
          set-box!
          unbox

          %make-hash-table
          make-eq-hash-table
          make-hash-table
          alist->hash-table
          hash
          hash-by-identity
          string-hash
          string-ci-hash
          hash-table?
          hash-table->alist
          hash-table-copy
          hash-table-delete!
          hash-table-equivalence-function
          hash-table-exists?
          hash-table-fold
          hash-table-hash-function
          hash-table-keys
          hash-table-merge!
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-size
          hash-table-update!
          hash-table-update!/default
          hash-table-values
          hash-table-walk

          environment?
          run-so
          list-directory
          realpath

          wrap
          unwrap
          wrapped?
          wrapped-kind
          wrapped-set-print
          record-set-print

          void
          void?
          system
          unread-char
          urandom

          gc
          gc-manual-mode!
          gc-stats))
