;; a stand-in for whisper's (whisper) library, so that main.scm can run
;; on another r7rs scheme and build the first whisper compiler of a
;; bootstrap.
;;
;; it provides the r7rs names plus the few whisper-specific names the
;; compiler sources use. only the parts that need something outside
;; r7rs are implementation specific, and those live in cond-expand.
(define-library (whisper)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme process-context)
          (scheme case-lambda)
          (scheme read)
          (scheme write)
          (scheme time))

  (cond-expand
   (guile
    (import (srfi srfi-69)
            (rename (only (guile)
                          system status:exit-val make-symbol canonicalize-path)
                    (system guile-system))
            (only (ice-9 ftw) scandir))
    (begin
      ;; guile's system returns the raw wait status, whisper's returns
      ;; the exit code.
      (define (system cmd)
        (status:exit-val (guile-system cmd)))

      ;; whisper's realpath returns #f for a missing file, but
      ;; canonicalize-path raises an error.
      (define (realpath path)
        (and (file-exists? path)
             (canonicalize-path path)))

      (define (list-directory dir)
        (and (file-exists? dir)
             (scandir dir)))

      ;; like whisper's, gensym makes uninterned symbols, so they can't
      ;; collide with a symbol of the same name in the source. r7rs has
      ;; no way to make one, but guile's make-symbol does.
      (define gensym-counter 0)
      (define (gensym . prefix)
        (let ((n gensym-counter))
          (set! gensym-counter (+ n 1))
          (make-symbol
           (string-append (if (null? prefix) "g" (car prefix))
                          (number->string n))))))))

  (begin
    ;; helpers, copied from lib/whisper/stdlib.scm
    (define (list* x . rest)
      (if (null? rest)
          x
          (cons x (apply list* rest))))

    (define (last ls)
      (list-ref ls (- (length ls) 1)))

    (define (filter pred ls)
      (let loop ((ls ls) (acc '()))
        (cond ((null? ls) (reverse acc))
              ((pred (car ls)) (loop (cdr ls) (cons (car ls) acc)))
              (else (loop (cdr ls) acc)))))

    (define (any? values)
      (cond ((null? values) #f)
            ((car values) #t)
            (else (any? (cdr values)))))

    (define (all? values)
      (cond ((null? values) #t)
            ((car values) (all? (cdr values)))
            (else #f)))

    (define (atom? x)
      (not (pair? x)))

    (define (!= m n)
      (not (= m n)))

    (define (sort lst less?)
      (define (merge a b)
        (cond ((null? a) b)
              ((null? b) a)
              ((less? (car b) (car a))
               (cons (car b) (merge a (cdr b))))
              (else
               (cons (car a) (merge (cdr a) b)))))
      (define (split lst)
        (if (or (null? lst) (null? (cdr lst)))
            (cons lst '())
            (let ((rest (split (cddr lst))))
              (cons (cons (car lst) (car rest))
                    (cons (cadr lst) (cdr rest))))))
      (if (or (null? lst) (null? (cdr lst)))
          lst
          (let ((halves (split lst)))
            (merge (sort (car halves) less?) (sort (cdr halves) less?)))))

    (define (string-join strs sep)
      (let loop ((result #f) (strs strs))
        (if (null? strs)
            (or result "")
            (if (not result)
                (loop (car strs) (cdr strs))
                (loop (string-append result sep (car strs)) (cdr strs))))))

    (define (string-split str sep)
      (if (string=? str "")
          '()
          (let ((len (string-length str)))
            (let loop ((start 0) (i 0) (acc '()))
              (cond ((= i len)
                     (reverse (cons (substring str start i) acc)))
                    ((char=? (string-ref str i) sep)
                     (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
                    (else
                     (loop start (+ i 1) acc)))))))

    (define (string-suffix? suffix str)
      (let ((suffix-len (string-length suffix))
            (str-len (string-length str)))
        (and (>= str-len suffix-len)
             (string=? suffix (substring str (- str-len suffix-len) str-len)))))

    ;; types
    (define-record-type <void> (make-void) void?)
    (define the-void (make-void))
    (define (void) the-void)

    (define (fixnum? x) (exact-integer? x))
    (define (flonum? x) (and (real? x) (inexact? x)))

    ;; the compiler only meets boxes when walking quoted data for datum
    ;; labels, and the host reader never produces whisper boxes.
    (define (box? x) #f)
    (define (unbox x) (error "unbox: not a box"))

    ;; hash tables. whisper's make-hash-table follows srfi 69, so the
    ;; srfi's own is re-exported as is.
    (define (make-eq-hash-table)
      (make-hash-table eq?))

    ;; the reader. the compiler parameterizes these around its calls to
    ;; read, but the host's read ignores them, so the compiled program
    ;; just has no source locations.
    (define (new-reader-state filename) #f)
    (define reader-state (make-parameter #f))
    (define reader-wrapper (make-parameter #f))

    (define (record-set-print type proc) #f)

    ;; only used by temp-filename below
    (define urandom-counter 0)
    (define (urandom n)
      (set! urandom-counter (+ urandom-counter 1))
      (let loop ((i 0)
                 (x (+ (* urandom-counter 1000003) (current-jiffy)))
                 (chars '()))
        (if (= i n)
            (list->string chars)
            (loop (+ i 1)
                  (quotient x 256)
                  (cons (integer->char (modulo x 256)) chars)))))

    ;; copied from lib/whisper/stdlib.scm
    (define (temp-filename)
      (let ((bytes (urandom 6)))
        (let loop ((i 0) (name "/tmp/"))
          (if (= i (string-length bytes))
              name
              (let ((c (char->integer (string-ref bytes i))))
                (loop (+ i 1)
                      (string-append name
                                     (if (< c 16) "0" "")
                                     (number->string c 16))))))))

    (define (run-so filename env)
      (error "run-so is not supported when bootstrapping from another scheme"))

    ;; used by format.scm
    (define %write-string write-string))

  ;; r7rs says this is relative to this file, but guile resolves it
  ;; against this file's path relative to the load path, opened from the
  ;; current directory. this file is at the top of the load path, so
  ;; that works out to the repo root when bootstrapping. see quirk 1 in
  ;; bootstrap-scheme.md.
  (include "lib/whisper/format.scm")

  ;; the r7rs names, from (scheme base), (scheme cxr), (scheme char),
  ;; (scheme case-lambda), (scheme file), (scheme process-context),
  ;; (scheme read), and (scheme write). lib/whisper/scheme-*-exports.scm
  ;; can't be reused here, because they rename-export a few names
  ;; (modulo, quotient, remainder) and guile rejects that.
  (export * + - ... / < <= = => > >= _ abs and append apply assoc assq assv
         begin binary-port? boolean=? boolean? bytevector bytevector-append
         bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref
         bytevector-u8-set! bytevector? caar cadr
         call-with-current-continuation call-with-port call-with-values
         call/cc car case cdar cddr cdr ceiling char->integer char-ready?
         char-whitespace? char<=? char<? char=? char>=? char>? char?
         close-input-port close-output-port close-port cond cons
         current-error-port current-input-port current-output-port define
         define-record-type define-syntax define-values do dynamic-wind else
         eof-object eof-object? eq? equal? eqv? error error-object?
         error-object-irritants error-object-message even? exact exact? expt
         file-error? floor floor/ floor-quotient floor-remainder
         flush-output-port for-each gcd get-output-bytevector
         get-output-string guard if include inexact inexact? input-port-open?
         input-port? integer->char integer? lambda lcm length let let*
         let-syntax let-values let*-values letrec letrec* letrec-syntax list
         list->string list->vector list-copy list-ref list-set! list-tail
         list? make-bytevector make-list make-parameter make-string
         make-vector map max member memq memv min modulo negative? newline not
         null? number->string number? open-input-bytevector open-input-string
         open-output-bytevector open-output-string odd? or output-port-open?
         output-port? pair? parameterize peek-char peek-u8 port? positive?
         procedure? quasiquote quotient quote raise raise-continuable
         read-bytevector read-bytevector! read-char read-line read-string
         read-u8 reverse remainder round set! set-car! set-cdr! square string
         string->list string->number string->symbol string->vector
         string-append string-copy string-copy! string-fill! string-for-each
         string-length string-map string-ref string-set! string<=? string<?
         string=? string>=? string>? string? substring symbol->string symbol=?
         symbol? syntax-rules textual-port? truncate truncate/
         truncate-quotient truncate-remainder u8-ready? unless unquote
         unquote-splicing values vector vector->list vector->string
         vector-append vector-copy vector-copy! vector-for-each vector-fill!
         vector-length vector-map vector-ref vector-set! vector? when
         with-exception-handler write-bytevector write-char write-string
         write-u8 zero? caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
         caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar
         cdaddr cddaar cddadr cdddar cddddr char-upcase char-ci<=? char-ci<?
         char-ci=? char-ci>=? char-ci>? char-downcase char-foldcase
         char-alphabetic? char-numeric? char-upper-case? char-lower-case?
         string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
         string-downcase string-foldcase string-upcase case-lambda
         call-with-input-file call-with-output-file open-binary-input-file
         open-binary-output-file open-input-file open-output-file delete-file
         with-input-from-file with-output-to-file command-line emergency-exit
         exit get-environment-variable read display write write-shared
         write-simple)

  (export list*
          last
          filter
          any?
          all?
          atom?
          !=
          sort
          string-join
          string-split
          string-suffix?
          format

          void
          void?
          fixnum?
          flonum?
          box?
          unbox

          make-hash-table
          make-eq-hash-table
          hash-table-set!
          hash-table-ref/default
          hash-table-keys
          hash-table-walk
          hash-table-fold

          new-reader-state
          reader-state
          reader-wrapper

          gensym
          record-set-print

          realpath
          list-directory
          system
          temp-filename
          run-so))
